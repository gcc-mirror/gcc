/* { dg-do run } */
/* { dg-options "-fsignaling-nans -mfpmath=sse -O2 -march=x86-64-v3 -mavx10.2" } */
/* { dg-require-effective-target avx10_2 } */

#define AVX10_2
#define AVX10_SCALAR
#define SIZE (128 / 32)
#include "avx10-helper.h"
#include <stdbool.h>
#include "avx10-minmax-helper.h"
#include "avx512f-mask-type.h"

void static
CALC (float *r, float *s1, float *s2, int R)
{
  r[0] = minmax_float(&s1[0], &s2[0], R);
  for(int i = 1; i < SIZE; i++)
    r[i] = s1[i];
}

void
TEST (void)
{
  int i, sign;
  UNION_TYPE (128, ) res1, res2, res3, src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  float res_ref[SIZE];

  SCALAR_UNIT_TEST(0, ss, , float);
  SCALAR_UNIT_TEST(1, ss, , float);
  SCALAR_UNIT_TEST(4, ss, , float);
  SCALAR_UNIT_TEST(5, ss, , float);
  SCALAR_UNIT_TEST(16, ss, , float);
  SCALAR_UNIT_TEST(17, ss, , float);
}
