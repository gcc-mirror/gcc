/* Test omitted parameter names not in C11: -pedantic-errors.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

void f (int) { } /* { dg-error "omitting parameter names" } */
