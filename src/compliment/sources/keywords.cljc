(ns compliment.sources.keywords
  "Completion for keywords interned globally across the application"
  (:require [compliment.sources :refer [defsource]]
            [compliment.utils :refer [defmemoized resolve-namespace]]
            )
  (:import #?(:clj java.lang.reflect.Field
              :cljr [System.Reflection BindingFlags FieldInfo])))

(defmemoized ^:private keywords-table
  []
  #?(:clj
     (let [^Field field (.getDeclaredField clojure.lang.Keyword "table")]
       (.setAccessible field true)
       (.get field nil))
     ;; see clojure-clr's test_helper.clj's get-field
     :cljr
     (let [^FieldInfo field (.GetField clojure.lang.Keyword "_symKeyMap"
                                       (enum-or BindingFlags/Public
                                                BindingFlags/NonPublic
                                                BindingFlags/DeclaredOnly
                                                BindingFlags/Instance
                                                BindingFlags/Static))]
       (into {}
             (reduce (fn [coll weak-ref]
                       (let [kwd (.Target weak-ref)]
                         (if (= (type kwd) clojure.lang.Keyword)
                           (conj coll [(name kwd) true]) ; jvm one has symbols
                           coll)))
                     []
                     ;; clojure clr's clojure.lang.JavaConcurrentDictionary
                     (.Values (.GetValue field nil)))))))

(defn- tagged-candidate [c]
  {:candidate c, :type :keyword})

(defn qualified-candidates
  "Returns a list of namespace-qualified double-colon keywords (like ::foo)
  resolved for the given namespace."
  [prefix ns]
  (let [prefix (subs prefix 2)
        ns-name (str ns)]
    (for [[kw _] (keywords-table)
          :when (= (namespace kw) ns-name)
          :when (#?(:clj .startsWith :cljr .StartsWith) (name kw) prefix)]
      (tagged-candidate (str "::" (name kw))))))

(defn namespace-alias-candidates
  "Returns a list of namespace aliases prefixed by double colon required in the
  given namespace."
  [prefix ns]
  (let [prefix (subs prefix 2)
        ns-name (str ns)]
    (for [[alias _] (ns-aliases ns)
          :let [aname (name alias)]
          :when (#?(:clj .startsWith :cljr .StartsWith) aname prefix)]
      (tagged-candidate (str "::" aname)))))

(defn aliased-candidates
  "Returns a list of alias-qualified double-colon keywords (like ::str/foo),
  where alias has to be registered in the given namespace."
  [prefix ns]
  (when-let [[_ alias prefix] (re-matches #"::([^/]+)/(.*)" prefix)]
    (let [alias-ns-name (str (resolve-namespace (symbol alias) ns))]
      (for [[kw _] (keywords-table)
            :when (= (namespace kw) alias-ns-name)
            :when (#?(:clj .startsWith :cljr .StartsWith) (name kw) prefix)]
        (tagged-candidate (str "::" alias "/" (name kw)))))))

(defn candidates
  [^String prefix, ns _]
  (let [single-colon? (#?(:clj .startsWith :cljr .StartsWith) prefix ":")
        double-colon? (#?(:clj .startsWith :cljr .StartsWith) prefix "::")
        has-slash? (> (#?(:clj .indexOf :cljr .IndexOf) prefix "/") -1)]
    (cond (and double-colon? has-slash?) (aliased-candidates prefix ns)
          double-colon? (concat (qualified-candidates prefix ns)
                                (namespace-alias-candidates prefix ns))
          single-colon? (for [[kw _] (keywords-table)
                              :when (#?(:clj .startsWith :cljr .StartsWith)
                                     (str kw) (subs prefix 1))]
                          (tagged-candidate (str ":" kw))))))

(defsource ::keywords
  :candidates #'candidates
  :doc (constantly nil))
