/* { dg-do run } */
/* { dg-options "-O2 -mavx512dq" } */
/* { dg-require-effective-target avx512dq } */

#define AVX512DQ
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

void
CALC (MASK_TYPE *r, int *s1)
{
  int i;
  MASK_TYPE res = 0;
  MASK_TYPE one = 1;

  for (i = 0; i < SIZE; i++)
    if (s1[i] >> 31)
      res = res | (one << i);

  *r = res;
}

void
TEST (void)
{
  int i, sign;
  UNION_TYPE (AVX512F_LEN, i_d) src;
  MASK_TYPE res, res_ref = 0;

  sign = -1;
  for (i = 0; i < SIZE; i++)
    {
      src.a[i] = 2 * i * sign;
      sign = sign * -1;
    }

  res = INTRINSIC (_movepi32_mask) (src.x);

  CALC (&res_ref, src.a);

  if (res_ref != res)
    abort ();
}
