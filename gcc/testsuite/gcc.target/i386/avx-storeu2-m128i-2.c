/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */
/* { dg-require-effective-target avx } */

#include "avx-check.h"

static void
avx_test (void)
{
  int e[12] = { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 };
  int f[12] = { -1, -18, 12, 0, 9, -1, 1, -9, 13, -24, -1, -1 };
  int i;
  __m256i x = _mm256_set_epi32 (1, -9, 13, -24, -18, 12, 0, 9);
  _mm256_storeu2_m128i ((__m128i_u *) (e + 1), (__m128i_u *) (e + 6), x);
  for (i = 0; i < 12; i++)
    if (e[i] != f[i])
      abort ();
}
