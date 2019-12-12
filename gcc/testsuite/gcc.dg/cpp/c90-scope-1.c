/* Test :: token not in C90.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c90 -pedantic-errors" } */

#define CONCAT(x, y) x ## y

CONCAT (:, :) /* { dg-error "does not give a valid preprocessing token" } */
