/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

static void
compute_movzxbw (unsigned char *s, short *r)
{
  int i;

  for (i = 0; i < 16; i++)
    r[i] = s[i];
}

static void
avx2_test (void)
{
  union128i_b s;
  union256i_w res;
  short res_ref[16];

  s.x = _mm_set_epi8 (1, 2, 3, 4, 200, 50, 6, 8, 1, 2, 3, 4, 200, 5, 6, 8);

  res.x = _mm256_cvtepu8_epi16 (s.x);

  compute_movzxbw (s.a, res_ref);

  if (check_union256i_w (res, res_ref))
    abort ();
}
