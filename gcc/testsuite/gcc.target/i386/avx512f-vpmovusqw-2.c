/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"
#include <limits.h>

void
CALC (unsigned short *r, unsigned long long *s, int mem)
{
  int i;
  int len = mem ? SIZE : 8;
  for (i = 0; i < len; i++)
    r[i] = (i < SIZE) ? ((s[i] > USHRT_MAX) ? USHRT_MAX : s[i]) : 0;
}

void
TEST (void)
{
  int i;
  UNION_TYPE (128, i_uw) res1, res2, res3;
  unsigned short res4[8];
  UNION_TYPE (AVX512F_LEN, i_uq) src;
  MASK_TYPE mask = MASK_VALUE;
  unsigned short res_ref[8];
  unsigned short res_ref2[8];

  for (i = 0; i < SIZE; i++)
    {
      src.a[i] = 1 + 34 * i;
      res2.a[i] = DEFAULT_VALUE;
      res4[i] = DEFAULT_VALUE;
    }

  for (i = SIZE; i < 8; i++)
    {
      res_ref2[i] = DEFAULT_VALUE * 2;
      res4[i] = DEFAULT_VALUE * 2;
    }

  res1.x = INTRINSIC (_cvtusepi64_epi16) (src.x);
  res2.x = INTRINSIC (_mask_cvtusepi64_epi16) (res2.x, mask, src.x);
  res3.x = INTRINSIC (_maskz_cvtusepi64_epi16) (mask, src.x);

  CALC (res_ref, src.a, 0);

  if (UNION_CHECK (128, i_uw) (res1, res_ref))
    abort ();

  MASK_MERGE (i_uw) (res_ref, mask, SIZE);
  if (UNION_CHECK (128, i_uw) (res2, res_ref))
    abort ();

  MASK_ZERO (i_uw) (res_ref, mask, SIZE);
  if (UNION_CHECK (128, i_uw) (res3, res_ref))
    abort ();

  INTRINSIC (_mask_cvtusepi64_storeu_epi16) (res4, mask, src.x);
  CALC (res_ref2, src.a, 1);

  MASK_MERGE (i_w) (res_ref2, mask, SIZE);
  if (checkVs (res4, res_ref2, 8))
    abort ();
}
