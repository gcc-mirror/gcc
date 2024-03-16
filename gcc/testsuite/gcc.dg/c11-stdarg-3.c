/* Test variadic functions with no named parameters not supported in C11, but
   diagnostic disabled with -Wno-c11-c23-compat.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors -Wno-c11-c23-compat" } */

int f (...);
int g (int (...));
int h (...) { return 0; }
