/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

void static
avx_test (void)
{
  int i;
  union256d u, s1;
  double e [4];

  s1.x = _mm256_set_pd (39578.467285, 7856.342941, 85632.783567, 47563.234215);
  u.x = _mm256_movedup_pd (s1.x);

  for (i = 0; i < 2; i++)
    e[2*i] = e[2*i+1] = s1.a[2*i];

  if (check_union256d (u, e))
    abort ();
}
