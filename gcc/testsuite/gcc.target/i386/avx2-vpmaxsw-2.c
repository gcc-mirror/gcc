/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

static void
compute_pmaxsw256 (short *s1, short *s2, short *r)
{
  int i;

  for (i = 0; i < 16; i++)
    r[i] = s1[i] > s2[i] ? s1[i] : s2[i];
}

static void
avx2_test (void)
{
  union256i_w s1, s2, res;
  short res_ref[16];
  int i, j, sign = 1;
  int fail = 0;

  for (i = 0; i < 10; i++)
    {
      for (j = 0; j < 16; j++)
	{
	  s1.a[j] = j * i * sign;
	  s2.a[j] = (i + 2000) * sign;
	  sign = -sign;
	}

      res.x = _mm256_max_epi16 (s1.x, s2.x);

      compute_pmaxsw256 (s1.a, s2.a, res_ref);

      fail += check_union256i_w (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
