/* Test error in C11 for no arguments passed for variable arguments to a
   macro.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#define M(X, ...) X

M (x); /* { dg-error "requires at least one argument" } */
M (x, y);
