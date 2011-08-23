/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

#define N 0xec

static void
compute_pshufd256 (int *s1, unsigned char imm, int *r)
{
  int i;

  for (i = 0; i < 4; i++)
    r[i] = s1[((N & (0x3 << (2 * i))) >> (2 * i))];

  for (i = 0; i < 4; i++)
    r[i + 4] = s1[((N & (0x3 << (2 * i))) >> (2 * i)) + 4];
}

void static
avx2_test (void)
{
  union256i_d s1, res;
  int res_ref[8];
  int i, j, sign = 1;
  int fail = 0;

  for (i = 0; i < 10; i++)
    {
      for (j = 0; j < 8; j++)
	{
	  s1.a[j] = j * i * sign;
	  sign = -sign;
	}

      res.x = _mm256_shuffle_epi32 (s1.x, N);
      compute_pshufd256 (s1.a, N, res_ref);

      fail += check_union256i_d (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
