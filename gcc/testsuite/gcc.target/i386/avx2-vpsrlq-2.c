/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

static void
compute_psrlq256 (long long int *s1, long long int *s2, long long int *r)
{
  int i;
  long long int count = s2[0];

  memset (r, 0, 32);

  if (count < 64)
    for (i = 0; i < 4; ++i)
      r[i] = s1[i] >> count;
}

void static
avx2_test (void)
{
  union256i_q s1, res;
  union128i_q s2;
  long long int res_ref[4];
  int i, j;
  int fail = 0;

  for (i = 0; i < 10; i++)
    {
      for (j = 0; j < 4; j++)
	s1.a[j] = j * i;

      s2.a[0] = i;

      res.x = _mm256_srl_epi64 (s1.x, s2.x);

      compute_psrlq256 (s1.a, s2.a, res_ref);

      fail += check_union256i_q (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
