/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

static void
CALC (unsigned *r, float *s)
{
  int i;
  for (i = 0; i < SIZE; i++)
    r[i] = (unsigned) (s[i] + 0.5);
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_ud) res1, res2, res3;
  UNION_TYPE (AVX512F_LEN,) src;
  MASK_TYPE mask = MASK_VALUE;
  unsigned res_ref[SIZE];

  for (i = 0; i < SIZE; i++)
    {
      src.a[i] = 1.5 + 34.67 * i;
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_cvtps_epu32) (src.x);
  res2.x = INTRINSIC (_mask_cvtps_epu32) (res2.x, mask, src.x);
  res3.x = INTRINSIC (_maskz_cvtps_epu32) (mask, src.x);

  CALC (res_ref, src.a);

  if (UNION_CHECK (AVX512F_LEN, i_ud) (res1, res_ref))
    abort ();

  MASK_MERGE (i_ud) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_ud) (res2, res_ref))
    abort ();

  MASK_ZERO (i_ud) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_ud) (res3, res_ref))
    abort ();
}
