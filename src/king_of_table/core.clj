(ns king-of-table.core
  (:require [clojure.string :as str]
            [java-time.api :as jt]))


(defn input->list
  [input-str]
  (->> (str/split input-str #", ")
    (shuffle)))


(def sample-list
  ["Alex Higgins" "Judd Trump" "Ronnie O'Sullivan" "Shaun Murphy"])


(def sample-log
  
  [{:players [0 3] 
    :start (jt/local-date-time "2025-07-10T01:05:36.123456")
    :end (jt/local-date-time "2025-07-10T01:07:04.862016")
    :winner 0}
   
   {:players [0 1]
    :start (jt/local-date-time "2025-07-10T01:07:04.862016")
    :end (jt/local-date-time "2025-07-10T01:08:31.764727")
    :winner 1}
   
   {:players [1 2]
    :start (jt/local-date-time "2025-07-10T01:08:31.764727")
    :ends (jt/local-date-time "2025-07-10T01:19:43.329166")
    :winner 1}
   
   
   ])


(defn ring-next
  [range n]

  (cond
    (> n (last range))
    (first range)
    
    :else
    (first (filter (fn [x] (> x n)) range))))




(defmacro with-safe-log
  [& body]
  `(try
     ~@body
     (catch Throwable e#
       (str (ex-message e#)))))


(defn players-range
  [list]
  (range (count list)))


(defn player-score
  [log seed-number]
  
  (let [participated-coll
        (filter (fn [x] (some #{seed-number} (:players x))) log)
        
        wins-coll
        (filter (fn [x] (= (:winner x) seed-number)) log)
        
        wins
        (count wins-coll)
        
        loses-coll
        (filter (fn [x] (not= (:winner x) seed-number)) participated-coll)
        
        loses
        (- (count participated-coll) wins)
        
        minutes-lost
        (map (fn [x] (jt/time-between (:start x) (:end x) :seconds)) loses-coll)
        
        ]
    
    {:seed seed-number
     :score (- wins loses)
     :participated participated-coll
     :loses loses-coll
     :wins wins-coll
     :minutes-lost (reduce + minutes-lost)}))


(defn next-up
  [-list log]
  (cond
    
    (= log [])
    [0 1]
    
    (< (inc (:round (last log))) (count -list))
    [(:winner (last log)) (inc (:round (last log)))]
    
    :else
    (let [winner
          (:winner (last log))
          
          needed-round
          (first 
            (filter 
              (fn [x] (= (:round x) (- (inc (:round (last log))) (dec (count -list))))) log))
          
          loser
          (first 
            (filter
              (fn [x] 
                (not= x (:winner needed-round))) 
              (:players needed-round)))]
      [winner loser])))


(defn format-next-up
  [list next-up-output]
  (format "%s \n%s" 
    (nth list (first next-up-output))
    (nth list (second next-up-output))))


(defn log->board
  [list log]
  
  (let [scores
        (map (fn [y] {:name (str (nth list y) (reduce str (repeat (- 30 (count (nth list y))) " "))) 
                      :score (format "% 4d"(:score (player-score log y)))
                      :minutes-lost (format "% 4d" (:minutes-lost (player-score log y)))
                      :participated (:participated (player-score log y))}) (players-range list))
        
        scores
        (sort-by (fn [x] (parse-long (str/trim (:score x)))) scores)
        
        scores
        (reverse scores)
        
        scores-str-map
        (map (fn [x y] (format "%s %s  %s сек \n" (:name x) (:score x) (:minutes-lost x))) scores (players-range list))]
    (reduce str scores-str-map)))


(defn safely-input-int
  [next-up]
  (let [n (with-safe-log (parse-long (str/trim (read-line))))]
    (cond
      (= n 1) (first next-up)
      (= n 2) (second next-up)
      :else (safely-input-int next-up))))


(defn -main
  []
  (println "Введите список игроков через запятую:")
  (let [->list 
        (read-line)
        
        game-list 
        (input->list ->list)
        
        ]
    
    (println game-list)
    (loop
      [live-log []
       round 1]
      (spit "log" live-log)
      (println)
      (println (log->board game-list live-log))
      (println)
      (println "Раунд " round)
      (println (format-next-up game-list (next-up game-list live-log)))
      (println "Победитель: ")
      (recur (conj live-log {:round round
                             :players (next-up game-list live-log)
                             :start (jt/local-date-time)
                             :winner (safely-input-int (next-up game-list live-log))
                             :end (jt/local-date-time)})
             (inc round))
      )
  ))



(comment
  
  (filter #{5} [1 2 3])
  
  (empty? [])
  
  (conj [{:a :b}] {:a :c})
  
  (format-next-up sample-list (next-up sample-list []))
  
  (nth [1 2 3] 1)
  
  (ring-next [0 3] 2)
  
  (first (filter (fn [x] (not= x 2) ) [2 3]))
  
  (next-up sample-list [])
  
  (parse-long (str/trim "1\n"))
  
  (sort-by 
    (fn [x] (parse-long (str/trim (:score x)))) 
      [{:score "   1"} {:score "   2"} {:score "   0"}])
  
  (some #{2} [2 3])
  (apply max [1 2])
  
  (reduce count [1 2 3 4])
  
  (players-range sample-list)
  
  (println (log->board sample-list sample-log))
  
  (next-up sample-list sample-log)
  
  (player-score sample-log 1)
  
  (str (jt/local-date-time))
  (input->list "Alex Higgins, Judd Trump, Ronnie O'Sullivan, Shaun Murphy, Stephen Hendry")
  
  
  )