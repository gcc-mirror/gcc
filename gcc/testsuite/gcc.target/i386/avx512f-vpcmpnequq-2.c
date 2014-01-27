/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

CALC (MASK_TYPE *r, long long *s1, long long *s2)
{
  int i;
  *r = 0;
  MASK_TYPE one = 1;

  for (i = 0; i < SIZE; i++)
    if (s1[i] != s2[i])
      *r = *r | (one << i);
}

void static
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_q) src1, src2;
  MASK_TYPE res1, res_ref;
  MASK_TYPE mask = MASK_VALUE;
  res1 = 0;

  for (i = 0; i < SIZE / 2; i++)
    {
      src1.a[i * 2] = i;
      src1.a[i * 2 + 1] = i * i;
      src2.a[i * 2] = 2 * i;
      src2.a[i * 2 + 1] = i * i;
    }

  res1 = INTRINSIC (_cmpneq_epu64_mask) (src1.x, src2.x);

  CALC (&res_ref, src1.a, src2.a);

  if (res1 != res_ref)
    abort ();
}
