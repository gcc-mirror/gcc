/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

void static
avx2_test (void)
{
  union256i_d u, s1, s2;
  int e[8];
  unsigned i;

  s1.x = _mm256_set_epi32 (100, 74, 50000, 4, 6999, 39999, 1000, 4);
  s2.x = _mm256_set_epi32 (88, 44, 33, 220, 4556, 2999, 2, 9000000);

  u.x = _mm256_add_epi32 (s1.x, s2.x);

  for (i = 0; i < 8; i++)
    e[i] = s1.a[i] + s2.a[i];

  if (check_union256i_d (u, e))
    abort ();
}
