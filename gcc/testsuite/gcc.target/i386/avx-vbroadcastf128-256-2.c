/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  union256d u;
  union128d s;
  double e [4];

  s.x = _mm_set_pd(24.43, 68.346);
  u.x = _mm256_broadcast_pd (&s.x);

  e[0] = e[2] = s.a[0];
  e[1] = e[3] = s.a[1];

  if (check_union256d (u, e))
    abort ();
}
