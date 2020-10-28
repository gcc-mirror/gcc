/* Test omitted parameter names in C2x: diagnosed with -Wc11-c2x-compat.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors -Wc11-c2x-compat" } */

void f (int) { } /* { dg-warning "omitting parameter names" } */
