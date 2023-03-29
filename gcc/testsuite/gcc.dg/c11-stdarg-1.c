/* Test variadic functions with no named parameters not supported in C11.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

int f (...); /* { dg-error "ISO C requires a named argument before" } */
int g (int (...)); /* { dg-error "ISO C requires a named argument before" } */
int h (...) { return 0; } /* { dg-error "ISO C requires a named argument before" } */
