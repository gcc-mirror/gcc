/* { dg-do run } */
/* { dg-options "-O2 -mavx512dq" } */
/* { dg-require-effective-target avx512dq } */

#define AVX512DQ
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"
#define IMM 0x02

void
CALC (float *s1, float *s2, float *r)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      if (s1[i] < -s2[i])
	r[i] = -s2[i];
      else if (s1[i] > s2[i])
	r[i] = s2[i];
      else
	r[i] = s1[i];
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, ) s1, s2, res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  float res_ref[SIZE];
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = 234.567 * i * sign;
      s2.a[i] = 100 * (i + 1);
      res2.a[i] = DEFAULT_VALUE;
      sign = -sign;
    }

  res1.x = INTRINSIC (_range_ps) (s1.x, s2.x, IMM);
  res2.x = INTRINSIC (_mask_range_ps) (res2.x, mask, s1.x, s2.x, IMM);
  res3.x = INTRINSIC (_maskz_range_ps) (mask, s1.x, s2.x, IMM);

  CALC (s1.a, s2.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, ) (res1, res_ref))
    abort ();

  MASK_MERGE () (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, ) (res2, res_ref))
    abort ();

  MASK_ZERO () (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, ) (res3, res_ref))
    abort ();
}
