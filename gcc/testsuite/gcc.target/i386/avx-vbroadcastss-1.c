/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  int i;
  float s = 39678.3452;
  union256 u;
  float e [8];

  u.x = _mm256_broadcast_ss (&s);

  for (i = 0; i < 8; i++)
    e[i] = s;

  if (check_union256 (u, e))
    abort ();
}
