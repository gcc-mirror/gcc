/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 16)
#define SIZE_HALF (AVX512F_LEN_HALF / 8)
#include "avx512f-mask-type.h"
#include <limits.h>

void
CALC (unsigned char *r, unsigned short *s)
{
  int i;
  for (i = 0; i < SIZE_HALF; i++)
      r[i] = (i < SIZE) ? ((s[i] > UCHAR_MAX) ? UCHAR_MAX : s[i]) : 0;
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN_HALF, i_b) res1, res2, res3;
  UNION_TYPE (AVX512F_LEN, i_w) src;
  MASK_TYPE mask = MASK_VALUE;
  unsigned char res_ref[32];

  for (i = 0; i < SIZE; i++)
    {
      src.a[i] = 1 + 34 * i;
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_cvtusepi16_epi8) (src.x);
  res2.x = INTRINSIC (_mask_cvtusepi16_epi8) (res2.x, mask, src.x);
  res3.x = INTRINSIC (_maskz_cvtusepi16_epi8) (mask, src.x);

  CALC (res_ref, src.a);

  if (UNION_CHECK (AVX512F_LEN_HALF, i_b) (res1, res_ref))
    abort ();

  MASK_MERGE (i_b) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN_HALF, i_b) (res2, res_ref))
    abort ();

  MASK_ZERO (i_b) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN_HALF, i_b) (res3, res_ref))
    abort ();
}
