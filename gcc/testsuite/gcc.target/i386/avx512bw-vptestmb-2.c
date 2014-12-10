/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 8)
#include "avx512f-mask-type.h"

void
CALC (MASK_TYPE *res, char *src1, char *src2)
{
  int i;
  *res = 0;
  MASK_TYPE one = 1;

  for (i = 0; i < SIZE; i++)
    if (src1[i] & src2[i])
      *res = *res | one << i;
}

void
TEST (void)
{
  int i, sign = 1;
  UNION_TYPE (AVX512F_LEN, i_b) src1, src2;
  MASK_TYPE res_ref, res1, res2;
  MASK_TYPE mask = MASK_VALUE;

  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = i * i * sign;
      src2.a[i] = i + 20;
      sign = -sign;
    }

  res1 = INTRINSIC (_test_epi8_mask) (src1.x, src2.x);
  res2 = INTRINSIC (_mask_test_epi8_mask) (mask, src1.x, src2.x);

  CALC (&res_ref, src1.a, src2.a);

  if (res1 != res_ref)
    abort ();

  res_ref &= mask;

  if (res2 != res_ref)
    abort ();
}
