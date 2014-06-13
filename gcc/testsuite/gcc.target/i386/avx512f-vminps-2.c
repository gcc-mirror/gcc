/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

static void
CALC (float *r, float *s1, float *s2)
{
  int i;
  for (i = 0; i < SIZE; i++)
    {
      r[i] = s1[i] < s2[i] ? s1[i] : s2[i];
    }
}

void
TEST (void)
{
  int i, sign;
  UNION_TYPE (AVX512F_LEN, ) res1, res2, res3, src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  float res_ref[SIZE];

  sign = -1;
  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = 1.5 + 34.67 * i * sign;
      src2.a[i] = -22.17 * i * sign;
      sign = sign * -1;
    }
  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_min_ps) (src1.x, src2.x);
  res2.x = INTRINSIC (_mask_min_ps) (res2.x, mask, src1.x, src2.x);
  res3.x = INTRINSIC (_maskz_min_ps) (mask, src1.x, src2.x);

  CALC (res_ref, src1.a, src2.a);

  if (UNION_CHECK (AVX512F_LEN, ) (res1, res_ref))
    abort ();

  MASK_MERGE () (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, ) (res2, res_ref))
    abort ();

  MASK_ZERO () (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, ) (res3, res_ref))
    abort ();
}
