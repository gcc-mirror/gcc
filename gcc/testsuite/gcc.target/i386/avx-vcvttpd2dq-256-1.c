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

  s1.x = _mm256_set_pd (2.43, 68.78, 23.61, 536.46);
  u.x = _mm256_cvttpd_epi32 (s1.x);

  for (i = 0; i < 4; i++)
    e[i] = (int)s1.a[i];

  if (check_union128i_d (u, e))
    abort ();
}
