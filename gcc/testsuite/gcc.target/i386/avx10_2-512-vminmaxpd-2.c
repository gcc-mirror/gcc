/* { dg-do run } */
/* { dg-options "-fsignaling-nans -mfpmath=sse -O2 -march=x86-64-v3 -mavx10.2" } */
/* { dg-require-effective-target avx10_2 } */

#ifndef AVX10_2
#define AVX10_2
#define AVX10_512BIT
#endif
#include "avx10-helper.h"
#define SIZE (AVX512F_LEN / 64)
#include <stdbool.h>
#include "avx10-minmax-helper.h"
#include "avx512f-mask-type.h"

void static
CALC (double *r, double *s1, double *s2, int R)
{
  for(int i = 0; i < SIZE; i++)
    r[i] = minmax_double(&s1[i], &s2[i], R);
}

void
TEST (void)
{
  int i, sign;
  UNION_TYPE (AVX512F_LEN, d) res1, res2, res3, src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  double res_ref[SIZE];

  UNIT_TEST(0, pd, d, double);
  UNIT_TEST(1, pd, d, double);
  UNIT_TEST(4, pd, d, double);
  UNIT_TEST(5, pd, d, double);
  UNIT_TEST(16, pd, d, double);
  UNIT_TEST(17, pd, d, double);
}
