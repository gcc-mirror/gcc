/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, ) res1, res2, res3, s2;
  MASK_TYPE mask = MASK_VALUE;
  float s1[SIZE];
  float res4[SIZE];
  float res5[SIZE];
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      s1[i] = 123.456 * (i + 2000) * sign;
      s2.a[i] = 789.012 * (i + 3000) * sign;
      res2.a[i] = DEFAULT_VALUE;
      res5[i] = DEFAULT_VALUE;
      sign = -sign;
    }

  res1.x = INTRINSIC (_loadu_ps) (s1);
  res2.x = INTRINSIC (_mask_loadu_ps) (res2.x, mask, s1);
  res3.x = INTRINSIC (_maskz_loadu_ps) (mask, s1);
  INTRINSIC (_storeu_ps) (res4, s2.x);
  INTRINSIC (_mask_storeu_ps) (res5, mask, s2.x);

  if (UNION_CHECK (AVX512F_LEN, ) (res1, s1))
    abort ();

  MASK_MERGE () (s1, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, ) (res2, s1))
    abort ();

  MASK_ZERO () (s1, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, ) (res3, s1))
    abort ();

  if (UNION_CHECK (AVX512F_LEN, ) (s2, res4))
    abort ();

  MASK_MERGE () (s2.a, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, ) (s2, res5))
    abort ();
}
