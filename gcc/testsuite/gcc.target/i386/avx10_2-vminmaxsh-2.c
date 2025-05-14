/* { dg-do run } */
/* { dg-options "-fsignaling-nans -mfpmath=sse -O2 -march=x86-64-v3 -mavx10.2" } */
/* { dg-require-effective-target avx10_2 } */

#define AVX10_2
#define AVX10_SCALAR
#define SIZE (128 / 16)
#include "avx10-helper.h"
#include <stdbool.h>
#include "avx10-minmax-helper.h"
#include "avx512f-mask-type.h"

void static
CALC (_Float16 *r, _Float16 *s1, _Float16 *s2, int R)
{
  r[0] = minmax__Float16(&s1[0], &s2[0], R);
  for(int i = 1; i < SIZE; i++)
    r[i] = s1[i];
}

void
TEST (void)
{
  int i, sign;
  UNION_TYPE (128, h) res1, res2, res3, src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  _Float16 res_ref[SIZE];

  SCALAR_UNIT_TEST(0, sh, h, _Float16);
  SCALAR_UNIT_TEST(1, sh, h, _Float16);
  SCALAR_UNIT_TEST(4, sh, h, _Float16);
  SCALAR_UNIT_TEST(5, sh, h, _Float16);
  SCALAR_UNIT_TEST(16, sh, h, _Float16);
  SCALAR_UNIT_TEST(17, sh, h, _Float16);
}
