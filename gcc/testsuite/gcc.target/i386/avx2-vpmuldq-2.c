/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

static void
compute_pmuldq256 (int *s1, int *s2, long long int *r)
{
  int i;

  for (i = 0; i < 4; i++)
    r[i] = s1[i * 2] * s2[i * 2];
}

static void
avx2_test (void)
{
  union256i_d s1, s2;
  union256i_q res;
  long long int res_ref[4];
  int i, j, sign = 1;
  int fail = 0;

  for (i = 0; i < 10; i++)
    {
      for (j = 0; j < 8; j++)
	{
	  s1.a[j] = i * j * sign;
	  s2.a[j] = (j + 20) * sign;
	  sign = -sign;
	}

      res.x = _mm256_mul_epi32 (s1.x, s2.x);
      compute_pmuldq256 (s1.a, s2.a, res_ref);

      fail += check_union256i_q (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
