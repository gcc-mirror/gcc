/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2-512" } */
/* { dg-require-effective-target avx10_2_512 } */

#ifndef AVX10_2
#define AVX10_2
#define AVX10_2_512
#define AVX10_512BIT
#endif

#include "avx10-helper.h"

#define SRC_SIZE (AVX512F_LEN / 16)
#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

static void  
CALC (float *dest, _Float16 *src1, _Float16 *src2)
{
  int i;

  for (i = 0; i < SIZE; i++) 
  {
    dest[i] += (float) src1[2 * i + 1] * (float) src2[2 * i + 1];
    dest[i] += (float) src1[2 * i] * (float) src2[2 * i];
  }
}

void
TEST(void)
{
  UNION_TYPE (AVX512F_LEN, h) src1, src2;
  UNION_TYPE (AVX512F_LEN,) res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  float res_ref[SIZE], res_ref2[SIZE], res_ref3[SIZE];

  for (int i = 0; i < SRC_SIZE; i++) 
  {
    src1.a[i] = (_Float16) (i * 4) + 1.25f16;
    src2.a[i] = (_Float16) (i * 2) + 2.5f16;
  }

  for (int i = 0; i < SIZE; i++)
  {
    res1.a[i] = 3.125f + 2 * i;
    res_ref[i] = 3.125f + 2 * i;
    res2.a[i] = DEFAULT_VALUE;
    res3.a[i] = DEFAULT_VALUE;
    res_ref2[i] = DEFAULT_VALUE;
    res_ref3[i] = DEFAULT_VALUE;
  }

  res1.x = INTRINSIC (_dpph_ps) (res1.x, src1.x, src2.x);
  res2.x = INTRINSIC (_mask_dpph_ps) (res2.x, mask, src1.x, src2.x);
  res3.x = INTRINSIC (_maskz_dpph_ps) (mask, res3.x, src1.x, src2.x);

  CALC(res_ref, src1.a, src2.a);
  CALC(res_ref2, src1.a, src2.a);
  CALC(res_ref3, src1.a, src2.a);

  if (UNION_CHECK(AVX512F_LEN,) (res1, res_ref))
    abort ();

  MASK_MERGE () (res_ref2, mask, SIZE);
  if (UNION_CHECK(AVX512F_LEN,) (res2, res_ref2))
    abort ();

  MASK_ZERO () (res_ref3, mask, SIZE);
  if (UNION_CHECK(AVX512F_LEN,) (res3, res_ref3))
    abort ();
}

