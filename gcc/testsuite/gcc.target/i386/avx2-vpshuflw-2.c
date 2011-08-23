/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

#define N 0xec

static void
compute_pshufhw256 (short *s1, unsigned char imm, short *r)
{
  int i;

  for (i = 0; i < 4; i++)
    {
      r[i] = s1[i];
      r[i + 8] = s1[i + 8];
    }

  for (i = 4; i < 8; i++)
    {
      r[i] = s1[((imm >> (2 * (i - 4))) & 3) + 4];
      r[i + 8] = s1[((imm >> (2 * (i - 4))) & 3) + 12];
    }
}

void static
avx2_test (void)
{
  union256i_w s1, res;
  short res_ref[16];
  int i, j, sign = 1;
  int fail = 0;

  for (i = 1; i < 10; i++)
    {
      for (j = 0; j < 16; j++)
	{
	  s1.a[j] = j * i * sign;
	  sign = -sign;
	}

      res.x = _mm256_shufflehi_epi16 (s1.x, N);
      compute_pshufhw256 (s1.a, N, res_ref);

      fail += check_union256i_w (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
