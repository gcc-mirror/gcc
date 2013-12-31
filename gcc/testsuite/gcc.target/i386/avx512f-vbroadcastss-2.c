/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

CALC (float *r, float *s)
{
  int i;
  for (i = 0; i < SIZE; i++)
    {
      r[i] = s[0];
    }
}

void static
TEST (void)
{
  int i, sign;
  UNION_TYPE (AVX512F_LEN,) res1, res2, res3;
  UNION_TYPE (128,) src;
  MASK_TYPE mask = MASK_VALUE;
  float res_ref[SIZE];

  sign = -1;
  for (i = 0; i < 4; i++)
    {
      src.a[i] = 1.5 + 34.67 * i * sign;
      sign = sign * -1;
    }
  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_broadcastss_ps) (src.x);
  res2.x = INTRINSIC (_mask_broadcastss_ps) (res2.x, mask, src.x);
  res3.x = INTRINSIC (_maskz_broadcastss_ps) (mask, src.x);

  CALC (res_ref, src.a);

  if (UNION_CHECK (AVX512F_LEN,) (res1, res_ref))
    abort ();

  MASK_MERGE ()(res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN,) (res2, res_ref))
    abort ();

  MASK_ZERO ()(res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN,) (res3, res_ref))
    abort ();
}
