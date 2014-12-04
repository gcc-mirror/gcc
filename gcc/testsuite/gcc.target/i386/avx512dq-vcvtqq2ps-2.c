/* { dg-do run } */
/* { dg-options "-O2 -mavx512dq" } */
/* { dg-require-effective-target avx512dq } */

#define AVX512DQ
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#define SIZE_HALF (AVX512F_LEN_HALF / 32)
#include "avx512f-mask-type.h"

void
CALC (long long *s, float *r)
{
  int i;

  for (i = 0; i < SIZE_HALF; i++)
    r[i] = (i < SIZE) ? (float) s[i] : 0;
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_q) s;
  UNION_TYPE (AVX512F_LEN_HALF,) res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  float res_ref[SIZE_HALF];
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      s.a[i] = 123 * (i + 2000) * sign;
      res2.a[i] = DEFAULT_VALUE;
      sign = -sign;
    }

  res1.x = INTRINSIC (_cvtepi64_ps) (s.x);
  res2.x = INTRINSIC (_mask_cvtepi64_ps) (res2.x, mask, s.x);
  res3.x = INTRINSIC (_maskz_cvtepi64_ps) (mask, s.x);

  CALC (s.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN_HALF,) (res1, res_ref))
    abort ();

  MASK_MERGE ()(res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN_HALF,) (res2, res_ref))
    abort ();

  MASK_ZERO ()(res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN_HALF,) (res3, res_ref))
    abort ();
}
