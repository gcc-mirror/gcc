/* { dg-do run } */
/* { dg-options "-O2 -mavx512dq" } */
/* { dg-require-effective-target avx512dq } */

#define AVX512DQ
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

#define IMM 0x23

void
CALC (float *s, float *r)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      float tmp = (int) (4 * s[i]) / 4.0;
      r[i] = s[i] - tmp;
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN,) s, res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  float res_ref[SIZE];
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      s.a[i] = 123.456 * (i + 2000) * sign;
      res2.a[i] = DEFAULT_VALUE;
      sign = -sign;
    }

  res1.x = INTRINSIC (_reduce_round_ps) (s.x, IMM, _MM_FROUND_TO_NEAREST_INT
					 | _MM_FROUND_NO_EXC);
  res2.x = INTRINSIC (_mask_reduce_round_ps) (res2.x, mask, s.x,
					      IMM, _MM_FROUND_TO_NEAREST_INT
					      | _MM_FROUND_NO_EXC);
  res3.x = INTRINSIC (_maskz_reduce_round_ps) (mask, s.x, IMM,
					       _MM_FROUND_TO_NEAREST_INT
					       | _MM_FROUND_NO_EXC);

  CALC (s.a, res_ref);

  if (UNION_FP_CHECK (AVX512F_LEN,) (res1, res_ref))
    abort ();

  MASK_MERGE () (res_ref, mask, SIZE);
  if (UNION_FP_CHECK (AVX512F_LEN,) (res2, res_ref))
    abort ();

  MASK_ZERO () (res_ref, mask, SIZE);
  if (UNION_FP_CHECK (AVX512F_LEN,) (res3, res_ref))
    abort ();
}
