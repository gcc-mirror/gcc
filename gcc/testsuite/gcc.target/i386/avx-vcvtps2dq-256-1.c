/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  int i;
  union256 s1;
  union256i_d u;
  int e [8];

  s1.x = _mm256_set_ps (2.78, 77768.82, 23.67, 536.46, 4564.6575, 568.1263, 9889.2422, 7352.4563);
  u.x = _mm256_cvtps_epi32 (s1.x);

  for (i = 0; i < 8; i++)
    e[i] = (int)(s1.a[i] + 0.5);

  if (check_union256i_d (u, e))
    abort ();
}
