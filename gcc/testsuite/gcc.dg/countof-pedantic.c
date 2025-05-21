/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic" } */

#include <stdcountof.h>

int a[1];
int b[countof(a)];
int c[_Countof(a)];  /* { dg-warning "ISO C does not support" } */
