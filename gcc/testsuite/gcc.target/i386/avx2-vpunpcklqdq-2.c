/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

#define N 0x5

static void
compute_punpcklqdq256 (long long int *s1, long long int *s2, long long int *r)
{
  r[0] = s1[0];
  r[1] = s2[0];
  r[2] = s1[2];
  r[3] = s2[2];
}

void static
avx2_test (void)
{
  union256i_q s1, s2, res;
  long long int res_ref[4];
  int i, j;
  int fail = 0;

  for (i = 0; i < 10; i++)
    {
      for (j = 0; j < 4; j++)
	{
	  s1.a[j] = j * i;
	  s2.a[j] = j + 20;
	}

      res.x = _mm256_unpacklo_epi64 (s1.x, s2.x);

      compute_punpcklqdq256 (s1.a, s2.a, res_ref);

      fail += check_union256i_q (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
