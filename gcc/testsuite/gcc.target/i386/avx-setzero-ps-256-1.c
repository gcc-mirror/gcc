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

  u.x = _mm256_setzero_ps ();

  for (i = 0; i < 8; i++)
    e[i] = 0.0;

  if (check_union256 (u, e))
    abort ();
}
