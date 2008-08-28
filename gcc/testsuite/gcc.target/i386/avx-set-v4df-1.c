/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

static __m256d
__attribute__((noinline))
foo (double *v)
{
  return _mm256_set_pd (v[3], v[2], v[1], v[0]);
}

static void
avx_test (void)
{
  double v[4] = { -3, 2, 1, 9 };
  union256d u;

  u.x = foo (v);
  if (check_union256d (u, v))
    abort ();
}
