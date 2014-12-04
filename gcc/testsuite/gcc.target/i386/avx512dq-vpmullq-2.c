/* { dg-do run } */
/* { dg-options "-O2 -mavx512dq" } */
/* { dg-require-effective-target avx512dq } */

#define AVX512DQ
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

void
CALC (long long *src1, long long *src2, long long *dst)
{
  int i;

  for (i = 0; i < SIZE; i++)
    dst[i] = src1[i] * src2[i];
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_q) src1, src2, dst1, dst2, dst3;
  long long dst_ref[SIZE];
  int i;
  MASK_TYPE mask = MASK_VALUE;

  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = i + 50;
      src2.a[i] = i + 100;
      dst2.a[i] = DEFAULT_VALUE;
    }

  dst1.x = INTRINSIC (_mullo_epi64) (src1.x, src2.x);
  dst2.x = INTRINSIC (_mask_mullo_epi64) (dst2.x, mask, src1.x, src2.x);
  dst3.x = INTRINSIC (_maskz_mullo_epi64) (mask, src1.x, src2.x);
  CALC (src1.a, src2.a, dst_ref);

  if (UNION_CHECK (AVX512F_LEN, i_q) (dst1, dst_ref))
    abort ();

  MASK_MERGE (i_q) (dst_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (dst2, dst_ref))
    abort ();

  MASK_ZERO (i_q) (dst_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (dst3, dst_ref))
    abort ();
}
