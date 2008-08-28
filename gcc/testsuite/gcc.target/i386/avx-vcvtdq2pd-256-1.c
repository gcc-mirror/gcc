/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  int i;
  union128i_d s1;
  union256d u;
  double e [4];

  s1.x = _mm_set_epi32 (2134.3343,1234.635654,453.345635,54646.464356);
  u.x = _mm256_cvtepi32_pd (s1.x);

  for (i = 0; i < 4; i++)
    e[i] = (double)s1.a[i];

  if (check_union256d (u, e))
    abort ();
}
