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

  s1.x = _mm256_set_ps (45.64, 4564.56, 2.3, 5.5, 57.57, 89.34, 54.12, 954.67);
  u.x = _mm256_cvttps_epi32 (s1.x);

  for (i = 0; i < 8; i++)
    e[i] = (int)s1.a[i];

  if (check_union256i_d (u, e))
    abort ();
}
