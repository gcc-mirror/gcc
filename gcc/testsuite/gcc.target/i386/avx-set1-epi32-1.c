/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  int i;
  union256i_d u;
  int e [8];
  int source[1] = {1234};

  u.x = _mm256_set1_epi32 (source[0]);

  for (i = 0; i < 8; i++)
    e[i] = source[0];

  if (check_union256i_d (u, e))
    abort ();
}
