/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */

#include "sse2-check.h"

typedef struct { __m128i f __attribute__((packed)); } packed;

__m128i  __attribute__((noinline))
foo (__m128i a1, __m128i a2, __m128i a3, __m128i a4,
     __m128i a5, __m128i a6, __m128i a7, __m128i a8,
     int b1, int b2, int b3, int b4, int b5, int b6, int b7, packed y)
{
  return y.f;
}

void
sse2_test (void)
{
  packed x;
  __m128i y = { 0 };
  x.f = y; 
  y = foo (y, y, y, y, y, y, y, y, 1, 2, 3, 4, 5, 6, -1, x);
  if (__builtin_memcmp (&y, &x.f, sizeof (y)) != 0)
    abort ();
}
