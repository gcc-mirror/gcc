/* { dg-do run } */
/* { dg-options "-mavx512f -O2 -mfpmath=sse -ffast-math" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F
#ifndef CHECK
#define CHECK "avx512f-helper.h"
#endif

#include CHECK

#include "pr98309-1.c"

double
__attribute__((noipa, target("fpmath=387")))
foo_i387 (double a, int b)
{
  return __builtin_ldexp (a, b);
}

float
__attribute__((noipa, target("fpmath=387")))
foo2_i387 (float a, int b)
{
  return __builtin_ldexpf (a, b);
}

static void
test_512 (void)
{
  float fa = 14.5;
  double da = 44.5;
  int fb = 12;
  int db = 8;
  if (foo_i387 (da, db) != foo (da, db))
    abort ();
  if (foo2_i387 (fa, fb) != foo2 (fa, fb))
    abort ();
}
