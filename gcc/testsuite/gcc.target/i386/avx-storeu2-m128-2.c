/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */
/* { dg-require-effective-target avx } */

#include "avx-check.h"

static void
avx_test (void)
{
  float e[12] = { -1.0f, -1.0f, -1.0f, -1.0f, -1.0f, -1.0f, -1.0f, -1.0f, -1.0f, -1.0f, -1.0f, -1.0f };
  float f[12] = { -1.0f, -18.75f, 12.0f, 0.0f, 9.0f, -1.0f, 1.5f, -9.5f, 13.25f, -24.75f, -1.0f, -1.0f };
  int i;
  __m256 x = _mm256_set_ps (1.5f, -9.5f, 13.25f, -24.75f, -18.75f, 12.0f, 0.0f, 9.0f);
  _mm256_storeu2_m128 (e + 1, e + 6, x);
  for (i = 0; i < 12; i++)
    if (e[i] != f[i])
      abort ();
}
