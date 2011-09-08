/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

static void
compute_movzxbq (unsigned char *s, long long int *r)
{
  int i;

  for (i = 0; i < 4; i++)
    r[i] = s[i];
}

static void
avx2_test (void)
{
  union128i_b s;
  union256i_q res;
  long long int res_ref[4];

  s.x = _mm_set_epi8 (1, 2, 3, 4, 20, 150, 6, 8, 1, 2, 3, 4, 20, 5, 6, 8);

  res.x = _mm256_cvtepu8_epi64 (s.x);

  compute_movzxbq (s.a, res_ref);

  if (check_union256i_q (res, res_ref))
    abort ();
}
