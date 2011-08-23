/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

static void
avx2_test (void)
{
  union256i_q u, s1, s2;
  long long int e[4];
  int i;

  s1.x = _mm256_set_epi64x (1, 2, 3, 4);

  s2.x = _mm256_set_epi64x (88, 44, 3, 220000);

  u.x = _mm256_cmpgt_epi64 (s1.x, s2.x);

  for (i = 0; i < 4; i++)
    e[i] = (s1.a[i] > s2.a[i]) ? -1 : 0;

  if (check_union256i_q (u, e))
    abort ();
}
