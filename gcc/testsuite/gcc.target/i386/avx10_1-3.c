/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -mavx10.1" } */

#include <immintrin.h>

int
foo (int c)
{
  register int a __asm ("k7") = c;
  int b = foo (a);
  asm volatile ("" : "+k" (b));
  return b;
}
