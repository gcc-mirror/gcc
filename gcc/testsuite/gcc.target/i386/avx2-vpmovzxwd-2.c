/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

static void
compute_movzxwd (unsigned short *s, int *r)
{
  int i;

  for (i = 0; i < 8; i++)
    r[i] = s[i];
}

static void
avx2_test (void)
{
  union128i_w s;
  union256i_d res;
  int res_ref[8];

  s.x = _mm_set_epi16 (1, 2, 3, 4, 200, 5000, 6, 8);

  res.x = _mm256_cvtepu16_epi32 (s.x);

  compute_movzxwd (s.a, res_ref);

  if (check_union256i_d (res, res_ref))
    abort ();
}
