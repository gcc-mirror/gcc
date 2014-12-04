/* { dg-do run } */
/* { dg-options "-O2 -mavx512dq" } */
/* { dg-require-effective-target avx512dq } */

#define AVX512DQ
#include "avx512f-helper.h"

#define SIZE    (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

void
CALC (float *src1, float *src2, float *dst)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      int tmp = (*(int *) &src1[i]) ^ (*(int *) &src2[i]);
      dst[i] = *(float *) &tmp;
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN,) s1, s2, res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  float dst_ref[SIZE];
  int i;

  for (i = 0; i < SIZE; i++) {
      s1.a[i] = 132.45 * i;
      s2.a[i] = 43.6 - i * 4.4;
      res2.a[i] = DEFAULT_VALUE;
  }

  res1.x = INTRINSIC (_xor_ps) (s1.x, s2.x);
  res2.x = INTRINSIC (_mask_xor_ps) (res2.x, mask, s1.x, s2.x);
  res3.x = INTRINSIC (_maskz_xor_ps) (mask, s1.x, s2.x);

  CALC (s1.a, s2.a, dst_ref);

  if (UNION_CHECK (AVX512F_LEN,) (res1, dst_ref))
    abort ();

  MASK_MERGE () (dst_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN,) (res2, dst_ref))
    abort ();

  MASK_ZERO () (dst_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN,) (res3, dst_ref))
    abort ();
}
