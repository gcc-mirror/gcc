/* { dg-do run } */
/* { dg-options "-O2 -mavx5124fmaps" } */
/* { dg-require-effective-target avx5124fmaps } */
/* { dg-warning "AVX5124FMAPS support will be removed in GCC 15" "" { target *-*-* } 0 } */

#define ESP_FLOAT 1.0

#define AVX5124FMAPS
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)

#include "avx512f-mask-type.h"

void
CALC (float *src1, float* src2, float *src3,
      float *src4, float* prev_dst, float *mult, float *dst)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      dst[i] = (double)prev_dst[i]
	- (double)src1[i] * (double)mult[0]
	- (double)src2[i] * (double)mult[1]
	- (double)src3[i] * (double)mult[2]
	- (double)src4[i] * (double)mult[3];
    }
}

void
TEST (void)
{
  int i, sign;
  UNION_TYPE (AVX512F_LEN,) src1, src2, src3, src4, src5, dst, res1, res2, res3;
  UNION_TYPE (128,) mult;
  MASK_TYPE mask = MASK_VALUE;
  float res_ref[SIZE];

  sign = -1;
  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = 1.5 + 34.67 * i * sign;
      src2.a[i] = -22.17 * i * sign;
      src3.a[i] = src1.a[i] * src1.a[i];
      src4.a[i] = src2.a[i] * src2.a[i];
      sign = sign * -1;
    }
  for (i = 0; i < 4; i++)
    mult.a[i] = 3.1415 + i * 2.71828;

  for (i = 0; i < SIZE; i++)
    src5.a[i] = DEFAULT_VALUE;

  CALC (src1.a, src2.a, src3.a, src4.a, src5.a, mult.a, res_ref);

  res1.x = INTRINSIC (_4fnmadd_ps)       (      src5.x, src1.x, src2.x, src3.x, src4.x, &mult.x);
  res2.x = INTRINSIC (_mask_4fnmadd_ps)  (src5.x, mask, src1.x, src2.x, src3.x, src4.x, &mult.x);
  res3.x = INTRINSIC (_maskz_4fnmadd_ps) (mask, src5.x, src1.x, src2.x, src3.x, src4.x, &mult.x);

  if (UNION_FP_CHECK (AVX512F_LEN,) (res1, res_ref))
    abort ();

  MASK_MERGE () (res_ref, mask, SIZE);
  if (UNION_FP_CHECK (AVX512F_LEN,) (res2, res_ref))
    abort ();

  MASK_ZERO () (res_ref, mask, SIZE);
  if (UNION_FP_CHECK (AVX512F_LEN,) (res3, res_ref))
    abort ();
}
