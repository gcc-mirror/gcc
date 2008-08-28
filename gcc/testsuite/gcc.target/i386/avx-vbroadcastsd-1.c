/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  int i;
  double s = 39678;
  union256d u;
  double e [4];

  u.x = _mm256_broadcast_sd (&s);

  for (i = 0; i < 4; i++)
    e[i] = s;

  if (check_union256d (u, e))
    abort ();
}
