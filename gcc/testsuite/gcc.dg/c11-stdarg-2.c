/* Test variadic functions with no named parameters not supported in C11.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic" } */

int f (...); /* { dg-warning "ISO C requires a named argument before" } */
int g (int (...)); /* { dg-warning "ISO C requires a named argument before" } */
int h (...) { return 0; } /* { dg-warning "ISO C requires a named argument before" } */
