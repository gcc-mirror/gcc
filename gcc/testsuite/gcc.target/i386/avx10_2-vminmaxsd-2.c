/* { dg-do run } */
/* { dg-options "-fsignaling-nans -mfpmath=sse -O2 -march=x86-64-v3 -mavx10.2" } */
/* { dg-require-effective-target avx10_2 } */

#define AVX10_2
#define AVX10_SCALAR
#define SIZE (128 / 64)
#include "avx10-helper.h"
#include <stdbool.h>
#include "avx10-minmax-helper.h"
#include "avx512f-mask-type.h"

void static
CALC (double *r, double *s1, double *s2, int R)
{
  r[0] = minmax_double(&s1[0], &s2[0], R);
  for(int i = 1; i < SIZE; i++)
    r[i] = s1[i];
}

void
TEST (void)
{
  int i, sign;
  UNION_TYPE (128, d) res1, res2, res3, src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  double res_ref[SIZE];

  SCALAR_UNIT_TEST(0, sd, d, double);
  SCALAR_UNIT_TEST(1, sd, d, double);
  SCALAR_UNIT_TEST(4, sd, d, double);
  SCALAR_UNIT_TEST(5, sd, d, double);
  SCALAR_UNIT_TEST(16, sd, d, double);
  SCALAR_UNIT_TEST(17, sd, d, double);
}
