/* { dg-do run } */
/* { dg-require-effective-target avx2 } */
/* { dg-options "-O2 -mavx2" } */

#include <string.h>
#include "avx2-check.h"

#define N 0x5

static void
compute_punpcklwd256 (int *s1, int *s2, int *r)
{
  int i;

  for (i = 0; i < 2; i++)
    {
      r[2 * i] = s1[i];
      r[2 * i + 1] = s2[i];

      r[2 * i + 4] = s1[i + 4];
      r[2 * i + 4 + 1] = s2[i + 4];
    }
}

void static
avx2_test (void)
{
  union256i_d s1, s2, res;
  int res_ref[8];
  int i, j;
  int fail = 0;

  for (i = 0; i < 10; i++)
    {
      for (j = 0; j < 8; j++)
	{
	  s1.a[j] = j * i;
	  s2.a[j] = j + 20;
	}

      res.x = _mm256_unpacklo_epi32 (s1.x, s2.x);

      compute_punpcklwd256 (s1.a, s2.a, res_ref);

      fail += check_union256i_d (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
