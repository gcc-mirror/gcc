/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

static void
CALC (double *r, double *s)
{
  int i;
  for (i = 0; i < SIZE; i++)
    {
      r[i] = s[i % 4];
    }
}

void
TEST (void)
{
  int i, sign;
  UNION_TYPE (AVX512F_LEN, d) res1, res2, res3;
  UNION_TYPE (256, d) src;
  MASK_TYPE mask = MASK_VALUE;
  double res_ref[SIZE];

  sign = -1;
  for (i = 0; i < 4; i++)
    {
      src.a[i] = 34.67 * i * sign;
      sign = sign * -1;
    }
  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_broadcast_f64x4) (src.x);
  res2.x = INTRINSIC (_mask_broadcast_f64x4) (res2.x, mask, src.x);
  res3.x = INTRINSIC (_maskz_broadcast_f64x4) (mask, src.x);

  CALC (res_ref, src.a);

  if (UNION_CHECK (AVX512F_LEN, d) (res1, res_ref))
    abort ();

  MASK_MERGE (d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res2, res_ref))
    abort ();

  MASK_ZERO (d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res3, res_ref))
    abort ();
}
