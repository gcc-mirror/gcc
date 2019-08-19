/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */
/* { dg-require-effective-target avx } */

#include "avx-check.h"

static void
avx_test (void)
{
  double e[8] = { -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0 };
  double f[8] = { -1.0, 13.25, -24.75, -1.0, 1.5, -9.5, -1.0, -1.0 };
  int i;
  __m256d x = _mm256_set_pd (1.5, -9.5, 13.25, -24.75);
  _mm256_storeu2_m128d (e + 1, e + 4, x);
  for (i = 0; i < 8; i++)
    if (e[i] != f[i])
      abort ();
}
