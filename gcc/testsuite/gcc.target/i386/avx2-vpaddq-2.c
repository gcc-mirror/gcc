/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

void static
avx2_test (void)
{
  union256i_q u, s1, s2;
  long long e[4];
  unsigned i;

  s1.x = _mm256_set_epi64x (100, 74, 50000, 4);
  s2.x = _mm256_set_epi64x (88, 44, 33, 220);

  u.x = _mm256_add_epi64 (s1.x, s2.x);

  for (i = 0; i < 4; i++)
    e[i] = s1.a[i] + s2.a[i];

  if (check_union256i_q (u, e))
    abort ();
}
