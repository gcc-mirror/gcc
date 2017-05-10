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
  union256i_d u, u2, u3, s1;
  union128i_d s2, s3;
  int e [8];

  s1.x = _mm256_set_epi32 (39467, 45789, 78342, 67892, 76678, 12963, 29746, 24753);
  s2.x = _mm_set_epi32 (57493, 38395, 22479, 31614);
  u.x = _mm256_insertf128_si256 (s1.x, s2.x, OFFSET);

  for (i = 0; i < 8; i++)
    e[i] = s1.a[i];

  for (i=0; i < 4; i++)
    e[i + (OFFSET * 4)] = s2.a[i];

  if (check_union256i_d (u, e))
    abort ();

  s3.x = _mm_set_epi32 (43534, 23235, 6545, 11);
  u2.x = _mm256_set_m128i(s3.x, s2.x);
  u3.x = _mm256_setr_m128i(s2.x, s3.x);

  for (i = 0; i < 4; i++)
    e[i] = s2.a[i];

  for (i = 0; i < 4; i++)
    e[i + 4] = s3.a[i];

  if (check_union256i_d (u2, e))
    abort ();

  if (check_union256i_d (u3, e))
    abort ();
}
