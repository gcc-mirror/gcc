/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

static void
compute_pmulhuw256 (unsigned short *s1, unsigned short *s2, unsigned short *r)
{
  int i;

  for (i = 0; i < 16; i++)
    r[i] = (s1[i] * s2[i]) >> 16;
}

static void
avx2_test (void)
{
  union256i_w s1, s2, res;
  unsigned short res_ref[16];
  int i, j;
  int fail = 0;

  for (i = 0; i < 10; i++)
    {
      for (j = 0; j < 16; j++)
	{
	  s1.a[j] = i * j;
	  s2.a[j] = j + 20;
	}

      res.x = _mm256_mulhi_epu16 (s1.x, s2.x);

      compute_pmulhuw256 (s1.a, s2.a, res_ref);

      fail += check_union256i_w (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
