/* Test omitted parameter names not in C11: -pedantic.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic" } */

void f (int) { } /* { dg-warning "omitting parameter names" } */
