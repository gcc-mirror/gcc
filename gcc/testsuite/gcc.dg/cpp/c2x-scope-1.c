/* Test :: token in C23.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#define CONCAT(x, y) x ## y

CONCAT (:, :)
CONCAT (::, >) /* { dg-error "does not give a valid preprocessing token" } */
