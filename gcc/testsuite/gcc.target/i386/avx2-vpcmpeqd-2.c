/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

static void
avx2_test (void)
{
  union256i_d u, s1, s2;
  int e[8];
  int i;

  s1.x = _mm256_set_epi32 (1, 2, 3, 4, 10, 20, 30, 90000);

  s2.x = _mm256_set_epi32 (88, 44, 3, 22, 11, 98, 76, -100);

  u.x = _mm256_cmpeq_epi32 (s1.x, s2.x);

  for (i = 0; i < 8; i++)
    e[i] = (s1.a[i] == s2.a[i]) ? -1 : 0;

  if (check_union256i_d (u, e))
    abort ();
}
