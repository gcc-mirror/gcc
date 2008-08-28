/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  int i;
  float s = 39678.3452;
  union128 u;
  float e [4];

  u.x = _mm_broadcast_ss (&s);

  for (i = 0; i < 4; i++)
    e[i] = s;

  if (check_union128 (u, e))
    abort ();
}
