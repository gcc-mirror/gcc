/* Test :: token not in C17.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c17 -pedantic-errors" } */

#define CONCAT(x, y) x ## y

CONCAT (:, :) /* { dg-error "does not give a valid preprocessing token" } */
CONCAT (::, >)
