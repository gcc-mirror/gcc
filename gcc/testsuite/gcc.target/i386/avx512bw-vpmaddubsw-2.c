/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#include <math.h>
#include <limits.h>
#include <float.h>
#define SIZE (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

void
CALC (short *i1, short *i2, short *r)
{
  unsigned char *ub1 = (unsigned char *) i1;
  char *sb2 = (char *) i2;
  short *sout = (short *) r;
  int t0;
  int i;

  for (i = 0; i < SIZE; i++)
    {
      t0 = ((int) ub1[2 * i] * (int) sb2[2 * i] +
	    (int) ub1[2 * i + 1] * (int) sb2[2 * i + 1]);
      if (t0 > (int) 0x7fff)
	sout[i] = 0x7fff;
      else if (t0 < (int) 0xffff8000)
	sout[i] = 0x8000;
      else
	sout[i] = (short) t0;
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_w) s1, s2, res1, res2, res3;
  short res_ref[SIZE];
  MASK_TYPE mask = MASK_VALUE;
  int i;
  int fail = 0;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = i * 17 + i;
      s2.a[i] = i * -17 + i * 2;
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_maddubs_epi16) (s1.x, s2.x);
  res2.x = INTRINSIC (_mask_maddubs_epi16) (res2.x, mask, s1.x, s2.x);
  res3.x = INTRINSIC (_maskz_maddubs_epi16) (mask, s1.x, s2.x);

  CALC(s1.a, s2.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_w) (res1, res_ref))
    abort ();

  MASK_MERGE (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res2, res_ref))
    abort ();

  MASK_ZERO (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res3, res_ref))
    abort ();
}
