/* Test omitted parameter names in C23: diagnosed with -Wc11-c23-compat.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors -Wc11-c23-compat" } */

void f (int) { } /* { dg-warning "omitting parameter names" } */
