/* { dg-do run } */
/* { dg-options "-O2 -mavx512cd" } */
/* { dg-require-effective-target avx512cd } */

#define AVX512CD

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

CALC (MASK_TYPE *res, long long *src1, long long *src2)
{
  int i;
  *res = 0;
  MASK_TYPE one = 1;

  for (i = 0; i < SIZE; i++)
    if (!(src1[i] & src2[i]))
      *res = *res | one << i;
}

static void
TEST (void)
{
  int i, sign = 1;
  UNION_TYPE (AVX512F_LEN, i_q) src1, src2;
  MASK_TYPE res_ref, res1, res2;
  MASK_TYPE mask = MASK_VALUE;
  res1 = 0;
  res2 = 0;

  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = i * i * sign;
      src2.a[i] = i + 20;
      sign = -sign;
    }

  res1 = INTRINSIC (_testn_epi64_mask) (src1.x, src2.x);
  res2 = INTRINSIC (_mask_testn_epi64_mask) (mask, src1.x, src2.x);

  CALC (&res_ref, src1.a, src2.a);

  if (res1 != res_ref)
    abort ();

  res_ref &= mask;

  if (res2 != res_ref)
    abort ();
}
