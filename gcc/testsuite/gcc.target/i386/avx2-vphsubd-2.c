/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"
#include "ssse3-vals.h"

static void
compute_phsubd256 (int *i1, int *i2, int *r)
{
  int i;

  for (i = 0; i < 2; i++)
    r[i + 0] = i1[2 * i] - i1[2 * i + 1];

  for (i = 0; i < 2; i++)
    r[i + 2] = i2[2 * i] - i2[2 * i + 1];

  for (i = 0; i < 2; i++)
    r[i + 4] = i1[2 * i + 4] - i1[2 * i + 5];

  for (i = 0; i < 2; i++)
    r[i + 6] = i2[2 * i + 4] - i2[2 * i + 5];
}

static void
avx2_test (void)
{
  union256i_d s1, s2, res;
  int res_ref[8];
  int i, j;
  int fail = 0;


  for (i = 0; i < 256; i += 16)
    {
      s1.x = _mm256_loadu_si256 ((__m256i *) & vals[i]);
      s2.x = _mm256_loadu_si256 ((__m256i *) & vals[i + 8]);

      res.x = _mm256_hsub_epi32 (s1.x, s2.x);

      compute_phsubd256 (s1.a, s2.a, res_ref);

      fail += check_union256i_d (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
