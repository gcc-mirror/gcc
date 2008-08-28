/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  int i;
  union256d u, s1, s2;
  double e [4];

  s1.x = _mm256_set_pd (2134.3343,1234.635654,453.345635,54646.464356);
  s2.x = _mm256_set_pd (41124.234,2344.2354,8653.65635,856.43576);
  u.x = _mm256_sub_pd (s1.x, s2.x);

  for (i = 0; i < 4; i++)
    e[i] = s1.a[i] - s2.a[i];

  if (check_union256d (u, e))
    abort ();
}
