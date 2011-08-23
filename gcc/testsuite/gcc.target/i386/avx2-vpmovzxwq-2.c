/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

static void
compute_movzxwq (unsigned short *s, long long int *r)
{
  int i;

  for (i = 0; i < 4; i++)
    r[i] = s[i];
}

static void
avx2_test (void)
{
  union128i_w s;
  union256i_q res;
  long long int res_ref[4];

  s.x = _mm_set_epi16 (1, 2, 3, 4, 200, 5000, 6, 8);

  res.x = _mm256_cvtepu16_epi64 (s.x);

  compute_movzxwq (s.a, res_ref);

  if (check_union256i_q (res, res_ref))
    abort ();
}
