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
  for (i = 0; i < SIZE/16; i++)
    {
      r[16 * i] = s1[16 * i];
      r[16 * i + 1] = s2[16 * i];
      r[16 * i + 2] = s1[16 * i + 1];
      r[16 * i + 3] = s2[16 * i + 1];
      r[16 * i + 4] = s1[16 * i + 2];
      r[16 * i + 5] = s2[16 * i + 2];
      r[16 * i + 6] = s1[16 * i + 3];
      r[16 * i + 7] = s2[16 * i + 3];
      r[16 * i + 8] = s1[16 * i + 4];
      r[16 * i + 9] = s2[16 * i + 4];
      r[16 * i + 10] = s1[16 * i + 5];
      r[16 * i + 11] = s2[16 * i + 5];
      r[16 * i + 12] = s1[16 * i + 6];
      r[16 * i + 13] = s2[16 * i + 6];
      r[16 * i + 14] = s1[16 * i + 7];
      r[16 * i + 15] = s2[16 * i + 7];
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
      src1.a[i] = 34 * i * sign;
      src1.a[i] = 179 * i;
      sign = sign * -1;
    }
  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_unpacklo_epi8) (src1.x, src2.x);
  res2.x = INTRINSIC (_mask_unpacklo_epi8) (res2.x, mask, src1.x, src2.x);
  res3.x = INTRINSIC (_maskz_unpacklo_epi8) (mask, src1.x, src2.x);

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
