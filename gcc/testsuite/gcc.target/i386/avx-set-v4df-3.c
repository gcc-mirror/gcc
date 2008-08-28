/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

static __m256d
__attribute__((noinline))
foo (double x)
{
  return _mm256_set_pd (x, x, x, x);
}

static void
avx_test (void)
{
  double e = 34.5;
  double v[4];
  union256d u;
  int i;

  for (i = 0; i < ARRAY_SIZE (v); i++)
    v[i] = e;
  u.x = foo (e);
  if (check_union256d (u, v))
    abort ();
}
