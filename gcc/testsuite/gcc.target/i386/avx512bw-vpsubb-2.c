/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 8)
#include "avx512f-mask-type.h"

void
CALC (char *r, char *s1, char *s2)
{
  int i;
  for (i = 0; i < SIZE; i++)
    {
      r[i] = s1[i] - s2[i];
    }
}

void
TEST (void)
{
  int i, sign;
  UNION_TYPE (AVX512F_LEN, i_b) res1, res2, res3, src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  char res_ref[SIZE];

  sign = -1;
  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = 2 + sign * 7 * i % 291;
      src2.a[i] = 3 + sign * 11 * (i % 377) * i;
      sign = sign * -1;
    }
  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_sub_epi8) (src1.x, src2.x);
  res2.x = INTRINSIC (_mask_sub_epi8) (res2.x, mask, src1.x, src2.x);
  res3.x = INTRINSIC (_maskz_sub_epi8) (mask, src1.x, src2.x);

  CALC (res_ref, src1.a, src2.a);

  if (UNION_CHECK (AVX512F_LEN, i_b) (res1, res_ref))
    abort ();

  MASK_MERGE (i_b) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_b) (res2, res_ref))
    abort ();

  MASK_ZERO (i_b) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_b) (res3, res_ref))
    abort ();
}
