/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  int i;
  union256 u;
  union128 s;
  float e [8];

  s.x = _mm_set_ps(24.43, 68.346, 43.35, 546.46);
  u.x = _mm256_broadcast_ps (&s.x);

  for (i = 0; i < 4; i++)
    e[i+4] = e[i] = s.a[i];

  if (check_union256 (u, e))
    abort ();
}
