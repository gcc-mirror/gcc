/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

#define iRoundMode 0x7

void static
avx_test (void)
{
  union256d u, s1;
  double source [4] = {2134.3343,1234.635654,453.345635,54646.464356};
  double e[4] = {0.0};
  int i;

  s1.x = _mm256_loadu_pd (source);
  u.x = _mm256_round_pd (s1.x, iRoundMode);

  for (i = 0; i < 4; i++)
    {
      __m128d tmp = _mm_load_sd (&s1.a[i]);
      tmp = _mm_round_sd (tmp, tmp, iRoundMode);
      _mm_store_sd (&e[i], tmp);
    }

  if (check_union256d (u, e))
    abort ();
}
