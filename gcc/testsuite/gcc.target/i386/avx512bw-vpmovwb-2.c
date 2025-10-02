/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 16)
#define SIZE_HALF (AVX512F_LEN_HALF / 8)
#include "avx512f-mask-type.h"

void
CALC (char *r, short *s)
{
  int i;
  for (i = 0; i < SIZE_HALF; i++)
    {
      r[i] = (i < SIZE) ? (char) s[i] : 0;
    }
}

void
TEST (void)
{
  int i, sign;
  UNION_TYPE (AVX512F_LEN_HALF, i_b) res1, res2, res3;
  char res4[SIZE];
  UNION_TYPE (AVX512F_LEN, i_w) src;
  MASK_TYPE mask = MASK_VALUE;
  char res_ref[32];
  char res_ref2[SIZE_HALF];

  sign = -1;
  for (i = 0; i < SIZE; i++)
    {
      src.a[i] = 1 + 34 * i * sign;
      sign = sign * -1;
      res2.a[i] = DEFAULT_VALUE;
      res4[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_cvtepi16_epi8) (src.x);
  res2.x = INTRINSIC (_mask_cvtepi16_epi8) (res2.x, mask, src.x);
  res3.x = INTRINSIC (_maskz_cvtepi16_epi8) (mask, src.x);

  CALC (res_ref, src.a);

  if (UNION_CHECK (AVX512F_LEN_HALF, i_b) (res1, res_ref))
    abort ();

  MASK_MERGE (i_b) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN_HALF, i_b) (res2, res_ref))
    abort ();

  MASK_ZERO (i_b) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN_HALF, i_b) (res3, res_ref))
    abort ();

  INTRINSIC (_mask_cvtepi16_storeu_epi8) (res4, mask, src.x);
  CALC (res_ref2, src.a);
  
  MASK_MERGE (i_b) (res_ref2, mask, SIZE);
  if (checkVc (res4, res_ref2, SIZE))
    abort (); 
}
