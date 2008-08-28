/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

static __m256
__attribute__((noinline, unused))
test (float *e)
{
  return _mm256_loadu_ps (e);
}

void static
avx_test (void)
{
  union256 u;
  float e [8]  = {24.43, 68.346, 43.35, 546.46, 46.79, 82.78, 82.7, 9.4};

  u.x = test (e);

  if (check_union256 (u, e))
    abort ();
}
