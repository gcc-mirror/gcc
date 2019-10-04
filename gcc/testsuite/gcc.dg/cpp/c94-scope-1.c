/* Test :: token not in C94.  */
/* { dg-do preprocess } */
/* { dg-options "-std=iso9899:199409 -pedantic-errors" } */

#define CONCAT(x, y) x ## y

CONCAT (:, :) /* { dg-error "does not give a valid preprocessing token" } */
CONCAT (::, >)
