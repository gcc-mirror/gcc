/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

static void
compute_psllvd128 (int *s1, int *s2, int *r)
{
  int i, count;

  for (i = 0; i < 4; ++i)
    {
      count = s2[i];
      r[i] = s1[i] << count;
    }
}

void static
avx2_test (void)
{
  union128i_d s1, s2, res;
  int res_ref[4];
  int i, j, sign = 1;
  int fail = 0;

  for (i = 0; i < 10; i++)
    {
      for (j = 0; j < 4; j++)
	{
	  s1.a[j] = j * i * sign;
	  s2.a[j] = (j + i) >> 2;
	  sign = -sign;
	}

      res.x = _mm_sllv_epi32 (s1.x, s2.x);

      compute_psllvd128 (s1.a, s2.a, res_ref);

      fail += check_union128i_d (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
