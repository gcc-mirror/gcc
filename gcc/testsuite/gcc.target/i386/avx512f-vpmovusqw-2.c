/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"
#include <limits.h>

CALC (unsigned short *r, unsigned long long *s)
{
  int i;
  for (i = 0; i < 8; i++)
    {
      r[i] = (s[i] > USHRT_MAX) ? USHRT_MAX : s[i];
      r[i] = (i < SIZE) ? r[i] : 0;
    }
}

void static
TEST (void)
{
  int i, sign;
  UNION_TYPE (128, i_w) res1, res2, res3;
  UNION_TYPE (AVX512F_LEN, i_q) src;
  MASK_TYPE mask = MASK_VALUE;
  unsigned short res_ref[8];

  for (i = 0; i < SIZE; i++)
    {
      src.a[i] = 1 + 34 * i;
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_cvtusepi64_epi16) (src.x);
  res2.x = INTRINSIC (_mask_cvtusepi64_epi16) (res2.x, mask, src.x);
  res3.x = INTRINSIC (_maskz_cvtusepi64_epi16) (mask, src.x);

  CALC (res_ref, src.a);

  if (UNION_CHECK (128, i_w) (res1, res_ref))
    abort ();

  MASK_MERGE (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (128, i_w) (res2, res_ref))
    abort ();

  MASK_ZERO (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (128, i_w) (res3, res_ref))
    abort ();
}
