/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"
#include "ssse3-vals.h"

static void
compute_pmaddwd256 (short *i1, short *i2, int *r)
{
  int i;

  for (i = 0; i < 8; i++)
    r[i] = ((int) i1[2 * i] * (int) i2[2 * i] +
	    (int) i1[2 * i + 1] * (int) i2[2 * i + 1]);
}

static void
avx2_test (void)
{
  union256i_w s1, s2;
  union256i_d res;
  int res_ref[8];
  int i;
  int fail = 0;

  for (i = 0; i < 256; i += 16)
    {
      s1.x = _mm256_loadu_si256 ((__m256i *) & vals[i]);
      s2.x = _mm256_loadu_si256 ((__m256i *) & vals[i + 8]);

      res.x = _mm256_madd_epi16 (s1.x, s2.x);

      compute_pmaddwd256 (s1.a, s2.a, res_ref);

      fail += check_union256i_d (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
