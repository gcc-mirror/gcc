/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

static void
CALC (double *s, double *r, MASK_TYPE mask)
{
  int i, k;

  for (i = 0, k = 0; i < SIZE; i++)
    {
      if (mask & (1 << i))
	r[i] = s[k++];
    }
}

static void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, d) s1, res2, res3, res4, res5;
  MASK_TYPE mask = MASK_VALUE;
  double s2[SIZE];
  double res_ref1[SIZE];
  double res_ref2[SIZE];
  double res_ref3[SIZE];
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = 123.456 * (i + 200) * sign;
      s2[i] = 789.012 * (i + 300) * sign;
      res2.a[i] = DEFAULT_VALUE;
      res4.a[i] = DEFAULT_VALUE;
      sign = -sign;
    }

  res2.x = INTRINSIC (_mask_expand_pd) (res2.x, mask, s1.x);
  res3.x = INTRINSIC (_maskz_expand_pd) (mask, s1.x);
  res4.x = INTRINSIC (_mask_expandloadu_pd) (res4.x, mask, s2);
  res5.x = INTRINSIC (_maskz_expandloadu_pd) (mask, s2);

  /* no mask is the same as all ones mask.  */
  CALC (s1.a, res_ref1, MASK_ALL_ONES);
  CALC (s1.a, res_ref2, mask);
  CALC (s2, res_ref3, mask);

  MASK_MERGE (d) (res_ref2, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res2, res_ref2))
    abort ();

  MASK_ZERO (d) (res_ref2, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res3, res_ref2))
    abort ();

  MASK_MERGE (d) (res_ref3, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res4, res_ref3))
    abort ();

  MASK_ZERO (d) (res_ref3, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res5, res_ref3))
    abort ();
}
