/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

static void
compute_psrld256 (int *s1, long long int *s2, int *r)
{
  int i;
  long long int count = s2[0];

  memset (r, 0, 32);

  if (count < 32)
    for (i = 0; i < 8; ++i)
      r[i] = s1[i] >> count;
}

void static
avx2_test (void)
{
  union256i_d s1, res;
  union128i_q s2;
  int res_ref[8];
  int i, j;
  int fail = 0;

  for (i = 0; i < 10; i++)
    {
      for (j = 0; j < 8; j++)
	s1.a[j] = j * i;

      s2.a[0] = i;

      res.x = _mm256_srl_epi32 (s1.x, s2.x);

      compute_psrld256 (s1.a, s2.a, res_ref);

      fail += check_union256i_d (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
