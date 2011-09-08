/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

#define N 0x5

static void
compute_punpcklwd256 (short *s1, short *s2, short *r)
{
  int i;

  for (i = 0; i < 4; i++)
    {
      r[2 * i] = s1[i];
      r[2 * i + 1] = s2[i];

      r[2 * i + 8] = s1[i + 8];
      r[2 * i + 8 + 1] = s2[i + 8];
    }
}

void static
avx2_test (void)
{
  union256i_w s1, s2, res;
  short res_ref[16];
  int i, j;
  int fail = 0;

  for (i = 0; i < 10; i++)
    {
      for (j = 0; j < 16; j++)
	{
	  s1.a[j] = j * i;
	  s2.a[j] = j + 20;
	}

      res.x = _mm256_unpacklo_epi16 (s1.x, s2.x);

      compute_punpcklwd256 (s1.a, s2.a, res_ref);

      fail += check_union256i_w (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
