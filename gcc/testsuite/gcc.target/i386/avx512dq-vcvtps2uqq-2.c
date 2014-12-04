/* { dg-do run } */
/* { dg-options "-O2 -mavx512dq" } */
/* { dg-require-effective-target avx512dq } */

#define AVX512DQ
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

void
CALC (unsigned long long *r, float *s)
{
  int i;
  for (i = 0; i < SIZE; i++)
    r[i] = (unsigned long long) (s[i] + ((s[i] >= 0) ? 0.5 : -0.5));
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_q) res1, res2, res3;
  UNION_TYPE (AVX512F_LEN_HALF,) src;
  MASK_TYPE mask = MASK_VALUE;
  unsigned long long res_ref[SIZE];

  for (i = 0; i < SIZE; i++)
    {
      res2.a[i] = DEFAULT_VALUE;
      src.a[i] = 1.5 + 34.67 * i;
    }

  res1.x = INTRINSIC (_cvtps_epu64) (src.x);
  res2.x = INTRINSIC (_mask_cvtps_epu64) (res2.x, mask, src.x);
  res3.x = INTRINSIC (_maskz_cvtps_epu64) (mask, src.x);

  CALC (res_ref, src.a);

  if (UNION_CHECK (AVX512F_LEN, i_q) (res1, res_ref))
    abort ();

  MASK_MERGE (i_q) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res2, res_ref))
    abort ();

  MASK_ZERO (i_q) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res3, res_ref))
    abort ();
}
