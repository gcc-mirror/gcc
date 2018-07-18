/* { dg-do run } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

#include "avx-check.h"

#ifndef OFFSET
#define OFFSET 0
#endif

#if OFFSET < 0 || OFFSET > 1
#error OFFSET must be within [0..1]
#endif

void static
avx_test (void)
{
  int i;
  union256 u, u2, u3, s1;
  union128 s2, s3;
  float e [8];

  s1.x = _mm256_set_ps (39.467, 45.789, 78.342, 67.892, 76.678, 12.963, 29.746, 24.753);
  s2.x = _mm_set_ps (57.493, 38.395, 22.479, 31.614);
  u.x = _mm256_insertf128_ps (s1.x, s2.x, OFFSET);

  for (i = 0; i < 8; i++)
    e[i] = s1.a[i];

  for (i=0; i < 4; i++)
    e[i + (OFFSET * 4)] = s2.a[i];

  if (check_union256 (u, e))
    abort ();

  s3.x = _mm_set_ps (435.435, 25.35, 324.76, 32.11);
  u2.x = _mm256_set_m128(s3.x, s2.x);
  u3.x = _mm256_setr_m128(s2.x, s3.x);

  for (i = 0; i < 4; i++)
    e[i] = s2.a[i];

  for (i = 0; i < 4; i++)
    e[i + 4] = s3.a[i];

  if (check_union256 (u2, e))
    abort ();

  if (check_union256 (u3, e))
    abort ();
}
