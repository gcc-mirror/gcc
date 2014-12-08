/* { dg-do run } */
/* { dg-options "-O2 -mavx512ifma" } */
/* { dg-require-effective-target avx512ifma } */

#define AVX512IFMA

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

void
CALC (long long *r, long long *s1, long long *s2, long long *s3)
{
  int i;
  long long a, b;

  for (i = 0; i < SIZE; i++)
    {
      /* Simulate higher 52 bits out of 104 bit,
	 by shifting opernads with 0 in lower 26 bits.  */
      a = s2[i] >> 26;
      b = s3[i] >> 26;
      r[i] = a * b + s1[i];
    }
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
      src1.a[i] = 15 + 3467 * i;
      src2.a[i] = 9217 + i;
      src1.a[i] = src1.a[i] << 26;
      src1.a[i] = src1.a[i] << 26;
      src1.a[i] &= ((1LL << 52) - 1);
      src2.a[i] &= ((1LL << 52) - 1);
      dst1.a[i] = DEFAULT_VALUE;
      dst2.a[i] = DEFAULT_VALUE;
      dst3.a[i] = DEFAULT_VALUE;
    }

  CALC (dst_ref, dst1.a, src1.a, src2.a);
  dst1.x = INTRINSIC (_madd52hi_epu64) (dst1.x, src1.x, src2.x);
  dst2.x = INTRINSIC (_mask_madd52hi_epu64) (dst2.x, mask, src1.x, src2.x);
  dst3.x = INTRINSIC (_maskz_madd52hi_epu64) (mask, dst3.x, src1.x, src2.x);

  if (UNION_CHECK (AVX512F_LEN, i_q) (dst1, dst_ref))
    abort ();

  MASK_MERGE (i_q) (dst_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (dst2, dst_ref))
    abort ();

  MASK_ZERO (i_q) (dst_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (dst3, dst_ref))
    abort ();
}
