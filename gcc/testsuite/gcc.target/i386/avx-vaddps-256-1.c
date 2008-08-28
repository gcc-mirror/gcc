/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  int i;
  union256 u, s1, s2;
  float e [8];

  s1.x = _mm256_set_ps (24.43, 68.346, 43.35, 546.46, 46.79, 82.78, 82.7, 9.4);
  s2.x = _mm256_set_ps (1.17, 2.16, 3.15, 4.14, 5.13, 6.12, 7.11, 8.9);
  u.x = _mm256_add_ps (s1.x, s2.x);

  for (i = 0; i < 8; i++)
    e[i] = s1.a[i] + s2.a[i];

  if (check_union256 (u, e))
    abort ();
}
