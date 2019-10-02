/* Test :: token in gnu11.  */
/* { dg-do preprocess } */
/* { dg-options "-std=gnu11 -pedantic-errors" } */

#define CONCAT(x, y) x ## y

CONCAT (:, :)
CONCAT (::, >) /* { dg-error "does not give a valid preprocessing token" } */
