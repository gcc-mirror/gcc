/* { dg-do run } */
/* { dg-options "-mavx2 -O2" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"
#include "ssse3-vals.h"

static short
signed_saturate_to_word (int x)
{
  if (x > (int) 0x7fff)
    return 0x7fff;

  if (x < (int) 0xffff8000)
    return 0x8000;

  return (short) x;
}

static void
compute_phsubsw256 (short *i1, short *i2, short *r)
{
  int i;

  for (i = 0; i < 4; i++)
    r[i + 0] = signed_saturate_to_word (i1[2 * i] - i1[2 * i + 1]);

  for (i = 0; i < 4; i++)
    r[i + 4] = signed_saturate_to_word (i2[2 * i] - i2[2 * i + 1]);

  for (i = 0; i < 4; i++)
    r[i + 8] = signed_saturate_to_word (i1[2 * i + 8] - i1[2 * i + 9]);

  for (i = 0; i < 4; i++)
    r[i + 12] = signed_saturate_to_word (i2[2 * i + 8] - i2[2 * i + 9]);
}

static void
avx2_test (void)
{
  union256i_w s1, s2, res;
  short res_ref[16];
  int i;
  int fail = 0;

  for (i = 0; i < 256; i += 16)
    {
      s1.x = _mm256_loadu_si256 ((__m256i *) & vals[i]);
      s2.x = _mm256_loadu_si256 ((__m256i *) & vals[i + 8]);

      res.x = _mm256_hsubs_epi16 (s1.x, s2.x);

      compute_phsubsw256 (s1.a, s2.a, res_ref);

      fail += check_union256i_w (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
