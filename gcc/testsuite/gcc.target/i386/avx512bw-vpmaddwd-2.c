/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#include <math.h>
#include <limits.h>
#include <float.h>
#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

void
CALC (short *i1, short *i2, int *r)
{
  int i;

  for (i = 0; i < SIZE; i++)
    r[i] = ((int) i1[2 * i] * (int) i2[2 * i] +
	    (int) i1[2 * i + 1] * (int) i2[2 * i + 1]);
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_w) s1, s2;
  UNION_TYPE (AVX512F_LEN, i_d) res1, res2, res3;
  int res_ref[SIZE];
  MASK_TYPE mask = MASK_VALUE;
  int i;

  for (i = 0; i < SIZE * 2; i++)
    {
      s1.a[i] = i * 17 + i;
      s2.a[i] = i * -17 + i * 2;
    }

  for (i = 0; i < SIZE; i++)
    {
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_madd_epi16) (s1.x, s2.x);
  res2.x = INTRINSIC (_mask_madd_epi16) (res2.x, mask, s1.x, s2.x);
  res3.x = INTRINSIC (_maskz_madd_epi16) (mask, s1.x, s2.x);

  CALC(s1.a, s2.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_d) (res1, res_ref))
    abort ();

  MASK_MERGE (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res2, res_ref))
    abort ();

  MASK_ZERO (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res3, res_ref))
    abort ();
}
