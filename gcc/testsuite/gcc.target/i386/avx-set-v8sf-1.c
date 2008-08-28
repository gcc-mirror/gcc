/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

static __m256 
__attribute__((noinline))
foo (float *v)
{
  return _mm256_set_ps (v[7], v[6], v[5], v[4],
			v[3], v[2], v[1], v[0]);
}

static void
avx_test (void)
{
  float v[8] = { -3, 2, 1, 9, 23, -173, -13, 69 };
  union256 u;

  u.x = foo (v);
  if (check_union256 (u, v))
    abort ();
}
