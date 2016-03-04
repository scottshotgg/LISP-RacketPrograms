#|

Made by Scott Gaydos on 3/4/2015
CS/CE 4337.001
Programming Project 1 - Racket

All tests are included below the functions themselves. The comment above the function describes what 
the function does and a little bit on how I implemented it.

|#


#lang racket

;------------------------------------------------------------------------------------------------------

#|

#( 1. my-reverse )

my-reverse uses a helper function to create two lists, one of which is the original and the other 
is the reverselist that is returned. It reverse the list by cons-ing the original list onto the 
reverselist.

|#

(define my-reverse-helper
  (lambda (mylist reverselist)
    (if (= 1 (length mylist))
        (cons (car mylist) reverselist)
      (begin
        (my-reverse-helper (cdr mylist) (cons (car mylist) reverselist))
      )
    )
  )
)

(define my-reverse
  (lambda (mylist)
    (define reverselist '())
    (my-reverse-helper mylist reverselist)
  )
)

;#(my-reverse '(0 "1" 2 "3" 4 "5" 6 "7" 8 "9" 10 "this is a cool string" (2 4 6 8 10)))
;(my-reverse '(0 "1" 2 "3" 4 "5" 6 "7" 8 "9" 10 "this is a cool string" (2 4 6 8 10)))

;------------------------------------------------------------------------------------------------------

#|

#( 2. my-map )

my-map takes in a function and applies the function to the each member in the list. This does 
not use a helper function.

|#

(define maplist '())

(define my-map
  (lambda (function mylist)
    (if (= 1 (length mylist))
      (cons (function (car mylist)) (cdr mylist))
      (begin
        (cons (function (car mylist)) (my-map function (cdr mylist)))
      )
    )
  )
)

;#(my-map sqrt '(1 2 3 4 5 6 7 8 9 0))
;(my-map sqrt '(1 2 3 4 5 6 7 8 9 0))

;------------------------------------------------------------------------------------------------------

#|

#( 3. function-3)

function-3 takes a function and applies it to the atom 3.

|#

(define function-3
  (lambda (function)
    (function 3)
  )
)
;#(function-3 sqrt)
;(function-3 sqrt)

;------------------------------------------------------------------------------------------------------

#|

#( 4. zipper )

zipper takes two lists and interleves their members together into individual lists using a helper that takes
the two lists and cons-s the members together into a list and then cons-s that onto a finallist, which is
returned from the funtion. At the end, the previous my-reverse function is used to reverse the list since 
by default it is in reverse order. 

|#

(define zipper-helper
  (lambda (firstList secondList finallist)
    (if (or (= 1 (length firstList)) (= 1 (length secondList)))
      (begin
        (cons (cons (car firstList) (cons (car secondList) '())) finallist)
      )
      (begin
        (zipper-helper (cdr firstList) (cdr secondList) (cons (cons (car firstList) (cons (car secondList) '())) finallist))
      )
    )
  )
)

(define zipper
  (lambda (firstList secondList)
    (my-reverse (zipper-helper firstList secondList '()))   ; hehe, using my own reverse to reverse my list
  )
)

#|
#(zipper '(1 (5 6) 2 3 4 5 "this is a cool string") '(6 7 8 "hi" 9 10 11))
(zipper '(1 (5 6) 2 3 4 5 "this is a cool string") '(6 7 8 "hi" 9 10 11))
#(zipper '(1 2 3 4 5) '(6 7 8 9 10))
(zipper '(1 2 3 4 5) '(6 7 8 9 10))
|#

;------------------------------------------------------------------------------------------------------

#| 

#(even)

Auxiliary even function that was used in place of the built in 'even?' function
It just takes a number and does the mod 2 of that number and depending on whether
the remainder is 1, -1, or 0 it returns false or true, respectively.

|#

(define even
  (lambda (integer)
    (define determine (remainder integer 2))
    (cond[(= 1 determine) (car (cons #f '()))]
         [(= 0 determine) (car (cons #t '()))]
         [(= -1 determine) (car (cons #f '()))]
    )
  )
)

;------------------------------------------------------------------------------------------------------

#|

#( 5. segregate )

segregate takes a list and splits it into two lists; one for odd numbers and one for even numbers. It uses 
a helper function that keeps track of the lists.

|#

(define segregate-helper
  (lambda (integerList evenList oddList)
    (if (= 1 (length integerList))
      (begin
        (if (even (car integerList))        
            (cons (car integerList) evenList)
            (cons (car integerList) oddList)
        )
        (cons evenList (cons oddList '()))
      )

      (begin
        (if (even (car integerList))
          (begin
            (segregate-helper (cdr integerList) (append evenList integerList) oddList)
          )  
            (segregate-helper (cdr integerList) evenList (append oddList integerList))
        )
      )
    )
  )
)

(define segregate
  (lambda (integerList)
  (if (= 0 (length integerList))
    (cons '() (cons '() '()))
    (segregate-helper integerList '() '()))
  )
)


;(segregate '(0 1 2 3 4 5 6 7 8 9))
;(segregate '())

;------------------------------------------------------------------------------------------------------

#|

#( 6. is-member? )

is-member takes a list and an atom and determines if the list contains that atom or not. It uses a helper
to keep track of a found variable and then uses that found variable to track whether the item that we are 
looking for has been found. It is important to note about this function that it stops at the first sight of
the item that we are looking for.

|#

(define is-member-helper
  (lambda (found myInteger mylist)
    (if (= 1 (length mylist))
      (begin
        (if (equal? myInteger (car mylist))
          (car (cons #t '()))
          (begin
            (if (equal? #t found)
              (car (cons #t '()))
              (car (cons #f '()))
            )
          )
        )
      )
      (begin
        (if (equal? myInteger (car mylist))
          ;(is-member-helper #t myInteger (cdr mylist))
          (car (cons #t '()))
          (begin
            (if (equal? #t found)
              ;(is-member-helper #t myInteger (cdr mylist))
              (car (cons #t '()))
              (is-member-helper #f myInteger (cdr mylist))
            )
          )
        )
      )
    )
  )
)

(define is-member?
  (lambda (myInteger mylist)
    (if (= 0 (length mylist))
      (car (cons #f '()))
      (is-member-helper #f myInteger mylist))
  )
)

;#(is-member? "hi" '("hi" "hi" 7 6 4 3 8 56))
;(is-member? "hi" '("hi" 7 6 4 3 8 56))

;------------------------------------------------------------------------------------------------------

#|

#(7. my-sorted)

my-sorted takes a list and checks whether or not it is in acsending order or not. It uses a helper to
continue passing the sorted truth variable down and the returned when it is done. It starts by assuming
list is sorted and then figures out whether or not it is sorted by using a cascading and on the boolean
values that are recieved from the comparisons.

|#

(define my-sorted-helper
  (lambda (sorted mylist checklist)
    (if (= 1 (length mylist))
      (begin
        (if (equal? #t (string? (car mylist)))
          (begin
            (if (string>=? (car mylist) (car checklist))
              (car (cons (and #t sorted) '()))
              (car (cons (and #f sorted) '()))
            )
          )
          (begin
            (if (>= (car mylist) (car checklist))
              (car (cons (and #t sorted) '()))
              (car (cons (and #f sorted) '()))
            )
          )
        )
      )
      (begin
        (if (equal? 0 (length checklist))
          (my-sorted-helper #t (cdr mylist) (cons (car mylist) checklist))
          (begin
            (if (equal? #t (string? (car mylist)))
              (begin
                (if (string>=? (car mylist) (car checklist))
                  (my-sorted-helper (and #t sorted) (cdr mylist) (cons (car mylist) checklist))
                  (my-sorted-helper (and #f sorted) (cdr mylist) (cons (car mylist) checklist))
                )
              )
              (begin
                (if (>= (car mylist) (car checklist))
                  (my-sorted-helper (and #t sorted) (cdr mylist) (cons (car mylist) checklist))
                  (my-sorted-helper (and #f sorted) (cdr mylist) (cons (car mylist) checklist))
                )
              )
            )
          )
        )
      )
    )
  )
)

(define my-sorted
  (lambda (mylist)
    (if (>= (length mylist) 2)
      (my-sorted-helper #t mylist '())  ; assume that the lists are sorted until proven guilty
      (car (cons #t '()))
    )
    
  )
)
#|
#(my-sorted '())
(my-sorted '())
#(my-sorted '(2))
(my-sorted '(2))
#(my-sorted '(9 78 7 6 454))
(my-sorted '(9 78 7 6 454))
#(my-sorted '(1 2 3 4 5 6 7 10 9))
(my-sorted '(1 2 3 4 5 6 7 10 9))
#(my-sorted '("alpha" "beta" "gamma"))
(my-sorted '("alpha" "beta" "gamma"))
|#

;------------------------------------------------------------------------------------------------------

#|

#(8. my-flatten )

my-flatten takes a list and flattens it, meaning if the list has sublists then those members are now 
individual members of the parent list instead. It uses a helper function to keep track of the lists.

|#

(define my-flatten-helper
  (lambda (mylist finallist)
    (if (= 1 (length mylist))
      (begin
        (if (list? (car mylist))
          (append finallist (car mylist))
          (append finallist mylist)
        )
      )
      (begin
        ;(if (equal? #t (list? ))
        (if (list? (car mylist))
          (my-flatten-helper (cdr mylist) (append finallist (my-flatten (car mylist))))
          (my-flatten-helper (cdr mylist) (append finallist (list (car mylist))))
        )
      )
    )
  )
)

(define my-flatten
  (lambda (mylist)
    (my-flatten-helper mylist '())
  )
)

;#(my-flatten '(5 (4 5 6 7) 6 7 8 9))
;(my-flatten '(5 (4 5 6 7) 6 7 8 9))

;------------------------------------------------------------------------------------------------------

#|

#(9. threshold)

threshold takes in a list and then offloads to a helper that takes two lists and appends the qualifying
members of the first list onto the second.

|#

(define threshold-helper
  (lambda (mylist thresholdValue finallist)
    (if (= 1 (length mylist))
      (begin
        (if (<= thresholdValue (car mylist))
          (append finallist mylist)
          (cons (car finallist) cdr (finallist))
        )
      )
      (begin
        (if (<= thresholdValue (car mylist))
          (threshold-helper (cdr mylist) thresholdValue (append finallist mylist))
          (threshold-helper (cdr mylist) thresholdValue finallist)
        )
      )
    )
  )
)


(define threshold
  (lambda (mylist thresholdValue)
    (threshold-helper mylist thresholdValue '())
  )
)

;#(threshold '(9 8 7 6 4.3 99.0 100) 6.1)
;(threshold '(9 8 7 6 4.3 99.0 100) 6.1)

;------------------------------------------------------------------------------------------------------

#| 

#( 10. my-list-ref )

my-list-ref takes a list and an index value and returns the value at that index. If the index is 
larger or equal to the length of the list then "index out of bounds is printed".

|#

(define my-list-ref-helper
  (lambda (mylist index checkList)

    (if (= index (length checkList))
        (car mylist)
    (begin
      (my-list-ref-helper (cdr mylist) index (cons (car mylist) checkList))))
  )
)

(define my-list-ref
  (lambda (mylist index)
    (if (< index (length mylist))
    (my-list-ref-helper mylist index '())
    (print "index out of bounds"))
  )
)

;#(my-list-ref '(1 2 3 4) 4)
;(my-list-ref '(1 2 3 4) 4)

;------------------------------------------------------------------------------------------------------

#|

#( BONUS. deep-reverse)

deep-reverse is literally the exact same funtion as my-reverse except it checks whether or not the 
car of mylist is infact a list itself, in which case it subsequently calls my-reverse on that list
and then cons-s that onto the reverselist.

|#

(define deep-reverse-helper
  (lambda (mylist reverselist)
    (if (= 1 (length mylist))
      (begin
        (if (list? (car mylist))
          ;(deep-reverse-helper (cdr mylist) (cons (my-reverse (car mylist)) reverselist))
          (cons (my-reverse (car mylist)) reverselist)
          (cons (car mylist) reverselist)
        )
      )
      (begin
        (if (list? (car mylist))
          (deep-reverse-helper (cdr mylist) (cons (my-reverse (car mylist)) reverselist))
          (deep-reverse-helper (cdr mylist) (cons (car mylist) reverselist))
        )
      )
    )
  )
)

(define deep-reverse
  (lambda (mylist)
    (define reverselist '())
    (deep-reverse-helper mylist reverselist)
  )
)

;#(deep-reverse '(0 "1" 2 "3" 4 "5" 6 "7" 8 "9" 10 "this is a cool string" (2 4 6 8 10) (2 4 6 8 10)))
;(deep-reverse '(0 "1" 2 "3" 4 "5" 6 "7" 8 "9" 10 "this is a cool string" (2 4 6 8 10) (2 4 6 8 10)))
