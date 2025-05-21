/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors -Wc23-c2y-compat" } */

#include <stdcountof.h>

int a[1];
int b[countof(a)];
int c[_Countof(a)];  /* { dg-warning "ISO C does not support" } */
