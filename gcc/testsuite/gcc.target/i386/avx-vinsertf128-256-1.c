/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

#ifndef OFFSET
#define OFFSET 1
#endif

#if OFFSET < 0 || OFFSET > 1
#error OFFSET must be within [0..1]
#endif

void static
avx_test (void)
{
  int i;
  union256d u, s1;
  union128d s2;
  double e [4];

  s1.x = _mm256_set_pd (2134.3343,1234.635654,453.345635,54646.464356);
  s2.x = _mm_set_pd (68543.731254, 3452.578238);
  u.x = _mm256_insertf128_pd (s1.x, s2.x, OFFSET);

  for (i = 0; i < 4; i++)
    e[i] = s1.a[i];

  for (i=0; i < 2; i++)
    e[i + (OFFSET * 2)] = s2.a[i];

  if (check_union256d (u, e))
    abort ();
}
