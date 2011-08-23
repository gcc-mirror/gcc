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
compute_pmaddubsw256 (short *i1, short *i2, short *r)
{
  unsigned char *ub1 = (unsigned char *) i1;
  char *sb2 = (char *) i2;
  short *sout = (short *) r;
  int t0;
  int i;

  for (i = 0; i < 16; i++)
    {
      t0 = ((int) ub1[2 * i] * (int) sb2[2 * i] +
	    (int) ub1[2 * i + 1] * (int) sb2[2 * i + 1]);
      sout[i] = signed_saturate_to_word (t0);
    }
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

      res.x = _mm256_maddubs_epi16 (s1.x, s2.x);

      compute_pmaddubsw256 (s1.a, s2.a, res_ref);

      fail += check_union256i_w (res, res_ref);
    }

  if (fail != 0)
    abort ();
}
