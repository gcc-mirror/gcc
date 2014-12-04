/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 8)

#include "avx512f-mask-type.h"

void
CALC (unsigned char *src1, unsigned char *src2,
      unsigned char *dst)
{
  int i;

  for (i = 0; i < SIZE; i++)
    dst[i] = src1[i] < src2[i] ? src1[i] : src2[i];
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_b) src1, src2, res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  unsigned char res_ref[SIZE];

  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = i * i;
      src2.a[i] = i + 20;
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_min_epu8) (src1.x, src2.x);
  res2.x = INTRINSIC (_mask_min_epu8) (res2.x, mask, src1.x, src2.x);
  res3.x = INTRINSIC (_maskz_min_epu8) (mask, src1.x, src2.x);

  CALC (src1.a, src2.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_b) (res1, res_ref))
    abort ();

  MASK_MERGE (i_b) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_b) (res2, res_ref))
    abort ();

  MASK_ZERO (i_b) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_b) (res3, res_ref))
    abort ();
}
