/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -mavx10.1-512" } */

#include <immintrin.h>

long long
foo (long long c)
{
  register long long a __asm ("k7") = c;
  long long b = foo (a);
  asm volatile ("" : "+k" (b));
  return b;
}
