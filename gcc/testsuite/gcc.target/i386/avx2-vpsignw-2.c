/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

static void
compute_psignw256 (short int *s1, short int *s2, short int *r)
{
  int i;

  for (i = 0; i < 16; i++)
    if (s2[i] < 0)
      r[i] = -s1[i];
    else if (s2[i] == 0)
      r[i] = 0;
    else
      r[i] = s1[i];
}

void static
avx2_test (void)
{
  union256i_w s1, s2, res;
  short int res_ref[16];
  int i, j;
  int fail = 0;

  for (i = 0; i < 10; i++)
    {
      for (j = 0; j < 16; j++)
	{
	  s1.a[j] = j * i;
	  s2.a[j] = j + 20;
	}

      res.x = _mm256_sign_epi16 (s1.x, s2.x);
      compute_psignw256 (s1.a, s2.a, res_ref);

      fail += check_union256i_w (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
