/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  union256 u, s1, s2;
  float e [8];

  s1.x = _mm256_set_ps (24.43, 68.346, 43.35, 546.46, 46.79, 82.78, 82.7, 9.4);
  s2.x = _mm256_set_ps (1.17, 2.16, 3.15, 4.14, 5.13, 6.12, 7.11, 8.9);
  u.x = _mm256_unpacklo_ps (s1.x, s2.x);

  e[0] = s1.a[0];
  e[1] = s2.a[0];
  e[2] = s1.a[1];
  e[3] = s2.a[1];
  e[4] = s1.a[4];
  e[5] = s2.a[4];
  e[6] = s1.a[5];
  e[7] = s2.a[5];

  if (check_union256 (u, e))
    abort ();
}
