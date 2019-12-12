/* Test :: token in gnu17.  */
/* { dg-do preprocess } */
/* { dg-options "-std=gnu17 -pedantic-errors" } */

#define CONCAT(x, y) x ## y

CONCAT (:, :)
CONCAT (::, >) /* { dg-error "does not give a valid preprocessing token" } */
