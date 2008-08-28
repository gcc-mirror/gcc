/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  int i;
  union256d s1;
  union128i_d u;
  int e [4];

  s1.x = _mm256_set_pd (2.78, 7777768.82, 23.67, 536.46);
  u.x = _mm256_cvtpd_epi32 (s1.x);

  for (i = 0; i < 4; i++)
    e[i] = (int)(s1.a[i] + 0.5);

  if (check_union128i_d (u, e))
    abort ();
}
