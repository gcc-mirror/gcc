/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

static void
compute_psllvq256 (long long int *s1, long long int *s2, long long int *r)
{
  int i;
  long long int count;

  for (i = 0; i < 4; ++i)
    {
      count = s2[i];
      r[i] = s1[i] << count;
    }
}

void static
avx2_test (void)
{
  union256i_q s1, s2, res;
  long long int res_ref[4];
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

      res.x = _mm256_sllv_epi64 (s1.x, s2.x);

      compute_psllvq256 (s1.a, s2.a, res_ref);

      fail += check_union256i_q (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
