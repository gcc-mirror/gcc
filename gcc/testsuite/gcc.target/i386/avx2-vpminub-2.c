/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

static void
compute_pminub256 (unsigned char *s1, unsigned char *s2, unsigned char *r)
{
  int i;

  for (i = 0; i < 32; i++)
    r[i] = s1[i] < s2[i] ? s1[i] : s2[i];
}

static void
avx2_test (void)
{
  union256i_b s1, s2, res;
  unsigned char res_ref[32];
  int i, j;
  int fail = 0;

  for (i = 0; i < 10; i++)
    {
      for (j = 0; j < 32; j++)
	{
	  s1.a[j] = j * i;
	  s2.a[j] = i + 200;
	}

      res.x = _mm256_min_epu8 (s1.x, s2.x);

      compute_pminub256 ((unsigned char *) s1.a,
			 (unsigned char *) s2.a, (unsigned char *) res_ref);

      fail += check_union256i_b (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
