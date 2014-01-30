/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"
#include <limits.h>

CALC (unsigned char *r, unsigned long long *s, int mem)
{
  int i;
  int len = mem ? 8 : 16;
  for (i = 0; i < len; i++)
    {
      r[i] = (s[i] > UCHAR_MAX) ? UCHAR_MAX : s[i];
      r[i] = (i < SIZE) ? r[i] : 0;
    }
}

void static
TEST (void)
{
  int i, sign;
  UNION_TYPE (128, i_b) res1, res2, res3;
  unsigned char res4[16];
  UNION_TYPE (AVX512F_LEN, i_q) src;
  MASK_TYPE mask = MASK_VALUE;
  unsigned char res_ref[16];
  unsigned char res_ref2[16];

  for (i = 0; i < SIZE; i++)
    {
      src.a[i] = 1 + 34 * i;
      res2.a[i] = DEFAULT_VALUE;
      res4[i] = DEFAULT_VALUE;
    }

  for (i = SIZE; i < 16; i++)
    {
      res4[i] = DEFAULT_VALUE * 2;
      res_ref2[i] = DEFAULT_VALUE * 2;
    }

  res1.x = INTRINSIC (_cvtusepi64_epi8) (src.x);
  res2.x = INTRINSIC (_mask_cvtusepi64_epi8) (res2.x, mask, src.x);
  res3.x = INTRINSIC (_maskz_cvtusepi64_epi8) (mask, src.x);

  CALC (res_ref, src.a, 0);

  if (UNION_CHECK (128, i_b) (res1, res_ref))
    abort ();

  MASK_MERGE (i_b) (res_ref, mask, SIZE);
  if (UNION_CHECK (128, i_b) (res2, res_ref))
    abort ();

  MASK_ZERO (i_b) (res_ref, mask, SIZE);
  if (UNION_CHECK (128, i_b) (res3, res_ref))
    abort ();

  INTRINSIC (_mask_cvtusepi64_storeu_epi8) (res4, mask, src.x);

  CALC (res_ref2, src.a, 1);
  MASK_MERGE (i_b) (res_ref2, mask, SIZE);

  if (checkVc (res4, res_ref2, 16))
    abort ();

}
