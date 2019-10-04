/* Test :: token in gnu99.  */
/* { dg-do preprocess } */
/* { dg-options "-std=gnu99 -pedantic-errors" } */

#define CONCAT(x, y) x ## y

CONCAT (:, :)
CONCAT (::, >) /* { dg-error "does not give a valid preprocessing token" } */
