/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O3 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

#define N 0x5

static void
compute_psllqi256 (long long int *s1, long long int *r)
{
  int i;

  memset (r, 0, 32);

  if (N < 64)
    for (i = 0; i < 4; ++i)
      r[i] = s1[i] << N;
}


void static
avx2_test (void)
{
  union256i_q s1, res;
  long long int res_ref[4];
  int i, j;
  int fail = 0;

  for (i = 0; i < 10; i++)
    {
      for (j = 0; j < 4; j++)
	s1.a[j] = j * i;

      res.x = _mm256_slli_epi64 (s1.x, N);

      compute_psllqi256 (s1.a, res_ref);

      fail += check_union256i_q (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
