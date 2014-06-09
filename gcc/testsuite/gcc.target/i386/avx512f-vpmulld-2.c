/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"


static void
CALC (int *src1, int *src2, int *dst)
{
  int i;

  for (i = 0; i < SIZE; i++)
    dst[i] = src1[i] * src2[i];
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_d) src1, src2, res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  int dst_ref[SIZE];
  int i;

  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = i + 50;
      src2.a[i] = i + 100;
    }

  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_mullo_epi32) (src1.x, src2.x);
  res2.x = INTRINSIC (_mask_mullo_epi32) (res2.x, mask, src1.x, src2.x);
  res3.x = INTRINSIC (_maskz_mullo_epi32) (mask, src1.x, src2.x);

  CALC (src1.a, src2.a, dst_ref);

  if (UNION_CHECK (AVX512F_LEN, i_d) (res1, dst_ref))
    abort ();

  MASK_MERGE (i_d) (dst_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res2, dst_ref))
    abort ();

  MASK_ZERO (i_d) (dst_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res3, dst_ref))
    abort ();

}
