/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

CALC (MASK_TYPE *r, int *s1, int *s2)
{
  int i;
  *r = 0;
  MASK_TYPE one = 1;

  for (i = 0; i < SIZE; i++)
    if (s1[i] <= s2[i])
      *r = *r | (one << i);
}

void static
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_d) src1, src2;
  MASK_TYPE res_ref, res1;
  MASK_TYPE mask = MASK_VALUE;
  res1 = 0;

  for (i = 0; i < SIZE / 2; i++)
    {
      src1.a[i * 2] = i;
      src1.a[i * 2 + 1] = i * i;
      src2.a[i * 2] = 2 * i;
      src2.a[i * 2 + 1] = i * i;
    }

  res1 = INTRINSIC (_cmple_epu32_mask) (src1.x, src2.x);

  CALC (&res_ref, src1.a, src2.a);

  if (res_ref != res1)
    abort ();
}
