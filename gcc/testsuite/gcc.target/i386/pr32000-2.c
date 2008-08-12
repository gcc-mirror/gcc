/* { dg-do run } */
/* { dg-skip-if "" { ! { ilp32 && dfp } } } */
/* { dg-options "-O -msse2 -std=gnu99 -mpreferred-stack-boundary=2" } */

#include "sse2-check.h"

typedef struct { _Decimal128 f __attribute__((packed)); } packed;

_Decimal128 __attribute__((noinline))
foo (_Decimal128 a1, _Decimal128 a2, _Decimal128 a3, _Decimal128 a4,
     _Decimal128 a5, _Decimal128 a6, _Decimal128 a7, _Decimal128 a8,
     int b1, int b2, int b3, int b4, int b5, int b6, int b7, packed y)
{
  return y.f;
}

void
sse2_test (void)
{
  packed x;
  _Decimal128 y = -1;
  x.f = y;
  y = foo (0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, -1, x);
  if (__builtin_memcmp (&y, &x.f, sizeof (y)))
    abort ();
}
