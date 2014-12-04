/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

void
CALC (short *src1, short *src2, short *dst)
{
  int i;

  for (i = 0; i < SIZE; i++)
    dst[i] = (src1[i] * src2[i]) >> 16;
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_w) src1, src2, dst1, dst2, dst3;
  short dst_ref[SIZE];
  MASK_TYPE mask = MASK_VALUE;
  int i, sign = -1;

  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = i % 2;
      src2.a[i] = i * sign;
      dst2.a[i] = DEFAULT_VALUE;
      sign = -sign;
    }

  dst1.x = INTRINSIC (_mulhi_epu16) (src1.x, src2.x);
  dst2.x =
    INTRINSIC (_mask_mulhi_epu16) (dst2.x, mask, src1.x, src2.x);
  dst3.x = INTRINSIC (_maskz_mulhi_epu16) (mask, src1.x, src2.x);

  CALC (src1.a, src2.a, dst_ref);

  if (UNION_CHECK (AVX512F_LEN, i_w) (dst1, dst_ref))
    abort ();

  MASK_MERGE (i_w) (dst_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (dst2, dst_ref))
    abort ();

  MASK_ZERO (i_w) (dst_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (dst3, dst_ref))
    abort ();
}
