/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#include <stdcountof.h>

int a[1];
int b[countof(a)];
int c[_Countof(a)];  /* { dg-error "ISO C does not support" } */
