#include "avx10-helper.h"
#define SIZE (AVX512F_LEN / 32)
#include <stdbool.h>
#include "avx10-minmax-helper.h"
#include "avx512f-mask-type.h"

void static
CALC (float *r, float *s1, float *s2, int R)
{
  for(int i = 0; i < SIZE; i++)
    r[i] = minmax_float(&s1[i], &s2[i], R);
}

void
TEST (void)
{
  int i, sign;
  UNION_TYPE (AVX512F_LEN, ) res1, res2, res3, src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  float res_ref[SIZE];

  UNIT_TEST(0, ps, , float);
  UNIT_TEST(1, ps, , float);
  UNIT_TEST(4, ps, , float);
  UNIT_TEST(5, ps, , float);
  UNIT_TEST(16, ps, , float);
  UNIT_TEST(17, ps, , float);
}
