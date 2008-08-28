/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

__m256
__attribute__((noinline))
foo (float x1, float x2, float x3, float x4,
     float x5, float x6, float x7, float x8)
{
  return _mm256_set_ps (x1, x2, x3, x4, x5, x6, x7, x8);
}

void
avx_test (void)
{
  float v[8] = { -3, 2, 1, 9, 23, -173, -13, 69 };
  union256 u;

  u.x = foo (v[7], v[6], v[5], v[4], v[3], v[2], v[1], v[0]);

  if (check_union256 (u, v))
    abort ();
}
