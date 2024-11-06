/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2-512" } */
/* { dg-require-effective-target avx10_2_512 } */

#ifndef AVX10_2
#define AVX10_2
#define AVX10_2_512
#define AVX10_512BIT
#endif
#include "avx10-helper.h"
#include <stdint.h>

#define SIZE (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

static void
CALC (_Float16 *res_ref, float *src1, float *src2)
{
  float fp32;
  int i;
  for (i = 0; i < SIZE / 2; i++)
    {
      fp32 = (float) 2 * i + 7 + i * 0.5;
      res_ref[i] = fp32;
      src2[i] = fp32;
    }
  for (i = SIZE / 2; i < SIZE; i++)
    {
      fp32 = (float)2 * i + 7 + i * 0.5;
      res_ref[i] = fp32;
      src1[i - (SIZE / 2)] = fp32;
    }
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, h) res1, res2, res3;
  UNION_TYPE (AVX512F_LEN, ) src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  _Float16 res_ref[SIZE];

  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  CALC (res_ref, src1.a, src2.a);

  res1.x = INTRINSIC (_cvtx2ps_ph) (src1.x, src2.x);
  if (UNION_CHECK (AVX512F_LEN, h) (res1, res_ref))
    abort ();

  res2.x = INTRINSIC (_mask_cvtx2ps_ph) (res2.x, mask, src1.x, src2.x);
  MASK_MERGE (h) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, h) (res2, res_ref))
    abort ();

  res3.x = INTRINSIC (_maskz_cvtx2ps_ph) (mask, src1.x, src2.x);
  MASK_ZERO (h) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, h) (res3, res_ref))
    abort ();
}
