/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

static void
compute_movsxdq (int *s, long long int *r)
{
  int i;

  for (i = 0; i < 4; i++)
    r[i] = s[i];
}

static void
avx2_test (void)
{
  union128i_d s;
  union256i_q res;
  long long int res_ref[4];

  s.x = _mm_set_epi32 (1, -2, 3, 4);

  res.x = _mm256_cvtepi32_epi64 (s.x);

  compute_movsxdq (s.a, res_ref);

  if (check_union256i_q (res, res_ref))
    abort ();
}
