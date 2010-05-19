/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */

#include "sse2-check.h"

typedef  __m128i __attribute__((aligned(1))) unaligned;

__m128i  __attribute__((noinline))
foo (__m128i a1, __m128i a2, __m128i a3, __m128i a4,
     __m128i a5, __m128i a6, __m128i a7, __m128i a8,
     int b1, int b2, int b3, int b4, int b5, int b6, int b7, unaligned y)
{
  return y;
}

void
sse2_test (void)
{
  unaligned x;
  __m128i y = { 0 };
  x = y; 
  y = foo (y, y, y, y, y, y, y, y, 1, 2, 3, 4, 5, 6, -1, x);
  if (__builtin_memcmp (&y, &x, sizeof (y)) != 0)
    abort ();
}
