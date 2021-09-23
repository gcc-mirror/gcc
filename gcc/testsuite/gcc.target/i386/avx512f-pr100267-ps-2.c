/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

static void
CALC (float *s, float *r, MASK_TYPE mask)
{
  int i, k;

  for (i = 0, k = 0; i < SIZE; i++)
    {
      if (mask & (1 << i))
	r[i] = s[k++];
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, ) s1, res2, res3, res4, res5, res6, res7, res8, res9;
  MASK_TYPE mask = (1 << SIZE - 1) - 1;
  float s2[SIZE];
  float res_ref1[SIZE];
  float res_ref2[SIZE];
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = 123.456 * (i + 200) * sign;
      s2[i] = 789.012 * (i + 300) * sign;
      res2.a[i] = DEFAULT_VALUE;
      res6.a[i] = DEFAULT_VALUE;
      sign = -sign;
    }

  res3.x = INTRINSIC (_mask_expand_ps) (res2.x, MASK_ALL_ONES, s1.x);
  res4.x = INTRINSIC (_mask_expand_ps) (res2.x, 0, s1.x);
  res5.x = INTRINSIC (_mask_expand_ps) (res2.x, mask, s1.x);
  res7.x = INTRINSIC (_mask_expandloadu_ps) (res6.x, MASK_ALL_ONES, s2);
  res8.x = INTRINSIC (_mask_expandloadu_ps) (res6.x, 0, s2);
  res9.x = INTRINSIC (_mask_expandloadu_ps) (res6.x, mask, s2);

  CALC (s1.a, res_ref1, mask);
  CALC (s2, res_ref2, mask);

  if (UNION_CHECK (AVX512F_LEN, ) (res3, s1.a))
    abort ();

  if (UNION_CHECK (AVX512F_LEN, ) (res4, res2.a))
    abort ();

  MASK_MERGE () (res_ref1, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, ) (res5, res_ref1))
    abort ();

  if (UNION_CHECK (AVX512F_LEN, ) (res7, s2))
    abort ();

  if (UNION_CHECK (AVX512F_LEN, ) (res8, res6.a))
    abort ();

  MASK_MERGE () (res_ref2, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, ) (res9, res_ref2))
    abort ();
}
