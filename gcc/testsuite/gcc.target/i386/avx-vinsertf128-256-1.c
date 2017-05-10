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
  union256d u, u2, u3, s1;
  union128d s2, s3;
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

  s3.x = _mm_set_pd (435345.43535, 23235.316545);
  u2.x = _mm256_set_m128d(s3.x, s2.x);
  u3.x = _mm256_setr_m128d(s2.x, s3.x);

  for (i = 0; i < 2; i++)
    e[i] = s2.a[i];

  for (i = 0; i < 2; i++)
    e[i + 2] = s3.a[i];

  if (check_union256d (u2, e))
    abort ();

  if (check_union256d (u3, e))
    abort ();
}
