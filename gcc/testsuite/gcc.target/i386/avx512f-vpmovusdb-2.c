/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"
#include <limits.h>

static void
CALC (unsigned char *r, unsigned int *s)
{
  int i;
  for (i = 0; i < 16; i++)
    {
      r[i] = (s[i] > UCHAR_MAX) ? UCHAR_MAX : s[i];
      r[i] = (i < SIZE) ? r[i] : 0;
    }
}

void
TEST (void)
{
  int i;
  UNION_TYPE (128, i_ub) res1, res2, res3;
  unsigned char res4[16];
  UNION_TYPE (AVX512F_LEN, i_ud) src;
  MASK_TYPE mask = MASK_VALUE;
  unsigned char res_ref[16];

  for (i = 0; i < SIZE; i++)
    {
      src.a[i] = 1 + 34 * i;
      res2.a[i] = DEFAULT_VALUE;
      res4[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_cvtusepi32_epi8) (src.x);
  res2.x = INTRINSIC (_mask_cvtusepi32_epi8) (res2.x, mask, src.x);
  res3.x = INTRINSIC (_maskz_cvtusepi32_epi8) (mask, src.x);
  INTRINSIC (_mask_cvtusepi32_storeu_epi8) (res4, mask, src.x);

  CALC (res_ref, src.a);

  if (UNION_CHECK (128, i_ub) (res1, res_ref))
    abort ();

  MASK_MERGE (i_ub) (res_ref, mask, SIZE);
  if (UNION_CHECK (128, i_ub) (res2, res_ref))
    abort ();

  if (checkVuc (res4, res_ref, 16))
    abort ();

  MASK_ZERO (i_ub) (res_ref, mask, SIZE);
  if (UNION_CHECK (128, i_ub) (res3, res_ref))
    abort ();
}
