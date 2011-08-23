/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O3 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

#define N 0x5

static void
compute_psradi256 (int *s1, int *r)
{
  int i;

  memset (r, 0, 32);

  if (N < 32)
    for (i = 0; i < 8; ++i)
      r[i] = s1[i] >> N;
}


void static
avx2_test (void)
{
  union256i_d s1, res;
  int res_ref[8];
  int i, j;
  int fail = 0;

  for (i = 0; i < 10; i++)
    {
      for (j = 0; j < 8; j++)
	s1.a[j] = j * i;

      res.x = _mm256_srai_epi16 (s1.x, N);

      compute_psradi256 (s1.a, res_ref);

      fail += check_union256i_d (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
