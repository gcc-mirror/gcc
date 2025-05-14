/* { dg-do run } */
/* { dg-options "-fsignaling-nans -mfpmath=sse -O2 -march=x86-64-v3 -mavx10.2" } */
/* { dg-require-effective-target avx10_2 } */

#ifndef AVX10_2
#define AVX10_2
#define AVX10_512BIT
#endif
#include "avx10-helper.h"
#define SIZE (AVX512F_LEN / 16)
#include <stdbool.h>
#include "avx10-minmax-helper.h"
#include "avx512f-mask-type.h"

void static
CALC (__bf16 *r, __bf16 *s1, __bf16 *s2, int R)
{
  for(int i = 0; i < SIZE; i++)
    r[i] = minmax___bf16(&s1[i], &s2[i], R);
}

void
TEST (void)
{
  int i, sign;
  UNION_TYPE (AVX512F_LEN, bf16_bf) res1, res2, res3, src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  __bf16 res_ref[SIZE];

  UNIT_TEST(0, pbh, bf16_bf, __bf16);
  UNIT_TEST(1, pbh, bf16_bf, __bf16);
  UNIT_TEST(4, pbh, bf16_bf, __bf16);
  UNIT_TEST(5, pbh, bf16_bf, __bf16);
  UNIT_TEST(16, pbh, bf16_bf, __bf16);
  UNIT_TEST(17, pbh, bf16_bf, __bf16);
}
