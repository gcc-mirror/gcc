/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"
#define SIZE_HALF (AVX512F_LEN_HALF / 16)
#include <limits.h>

static void
CALC (unsigned short *r, unsigned int *s)
{
  int i;
  for (i = 0; i < SIZE_HALF; i++)
    {
      r[i] = (s[i] > USHRT_MAX) ? USHRT_MAX : s[i];
      r[i] = (i < SIZE) ? r[i] : 0;
    }
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN_HALF, i_uw) res1, res2, res3;
  unsigned short res4[SIZE_HALF];
  UNION_TYPE (AVX512F_LEN, i_ud) src;
  MASK_TYPE mask = MASK_VALUE;
  unsigned short res_ref[SIZE_HALF];

  for (i = 0; i < SIZE; i++)
    {
      src.a[i] = 1 + 34 * i;
      res2.a[i] = DEFAULT_VALUE;
      res4[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_cvtusepi32_epi16) (src.x);
  res2.x = INTRINSIC (_mask_cvtusepi32_epi16) (res2.x, mask, src.x);
  res3.x = INTRINSIC (_maskz_cvtusepi32_epi16) (mask, src.x);
  INTRINSIC (_mask_cvtusepi32_storeu_epi16) (res4, mask, src.x);

  CALC (res_ref, src.a);

  if (UNION_CHECK (AVX512F_LEN_HALF, i_uw) (res1, res_ref))
    abort ();

  MASK_MERGE (i_uw) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN_HALF, i_uw) (res2, res_ref))
    abort ();

  if (checkVus (res4, res_ref, SIZE_HALF))
    abort ();

  MASK_ZERO (i_uw) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN_HALF, i_uw) (res3, res_ref))
    abort ();
}
