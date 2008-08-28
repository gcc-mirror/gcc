/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  int i;
  union256 u;
  float e [8];
  float	 source[1] = {1234.234f};

  u.x = _mm256_set1_ps (source[0]);

  for (i = 0; i < 8; i++)
    e[i] = source[0];

  if (check_union256 (u, e))
    abort ();
}
