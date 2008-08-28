/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  int i;
  union128 s1;
  union256d u;
  double e [4];

  s1.x = _mm_set_ps (2.43, 68.46, 23.35, 536.46);
  u.x = _mm256_cvtps_pd (s1.x);

  for (i = 0; i < 4; i++)
    e[i] = (double)s1.a[i];

  if (check_union256d (u, e))
    abort ();
}
