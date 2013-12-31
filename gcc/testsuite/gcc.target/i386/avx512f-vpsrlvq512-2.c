/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#include <string.h>
#include "avx512f-check.h"

static void
compute_psrlvq512 (long long int *s1, long long int *s2, long long int *r)
{
  int i;
  long long int count;

  for (i = 0; i < 8; ++i)
    {
      count = s2[i];
      r[i] = ((unsigned long long) s1[i]) >> count;
    }
}

void static
avx512f_test (void)
{
  union512i_q s1, s2, res;
  long long int res_ref[8];
  int i, j, sign = 1;
  int fail = 0;

  for (i = 0; i < 10; i++)
    {
      for (j = 0; j < 8; j++)
	{
	  s1.a[j] = j * i * sign;
	  s2.a[j] = (j + i) >> 2;
	  sign = -sign;
	}

      res.x = _mm512_srlv_epi64 (s1.x, s2.x);

      compute_psrlvq512 (s1.a, s2.a, res_ref);

      fail += check_union512i_q (res, res_ref);
    }

  if (fail != 0)
    abort ();
}