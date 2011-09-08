/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

static void
compute_psrlw256 (short *s1, long long int *s2, short *r)
{
  int i;
  int count = s2[0];

  memset (r, 0, 32);

  if (count < 16)
    for (i = 0; i < 16; ++i)
      r[i] = s1[i] >> count;
}

void static
avx2_test (void)
{
  union256i_w s1, res;
  union128i_q s2;
  short res_ref[16];
  int i, j;
  int fail = 0;

  for (i = 0; i < 10; i++)
    {
      for (j = 0; j < 16; j++)
	s1.a[j] = j * i;

      s2.a[0] = i;

      res.x = _mm256_srl_epi16 (s1.x, s2.x);

      compute_psrlw256 (s1.a, s2.a, res_ref);

      fail += check_union256i_w (res, res_ref);

      if (fail)
	{
	  for (j = 0; j < 16; ++j)
	    printf ("%d <->%d\n", res.a[j], res_ref[j]);
	  abort ();
	}
    }

  if (fail != 0)
    abort ();
}
