/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

static __m256
__attribute__((noinline))
foo (float x)
{
  return _mm256_set_ps (x, x, x, x, x, x, x, x);
}

static void
avx_test (void)
{
  float e = 34.5;
  float v[8];
  union256 u;
  int i;

  for (i = 0; i < ARRAY_SIZE (v); i++)
    v[i] = e;
  u.x = foo (e);
  if (check_union256 (u, v))
    abort ();
}
