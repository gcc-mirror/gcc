/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

static void
CALC (int *s1, int *s2, int *r)
{
  int i;

  for (i = 0; i < SIZE; i++)
    r[i] = s1[i] | s2[i];
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_d) s1, s2, res1, res2, res3, res4;
  MASK_TYPE mask = MASK_VALUE;
  int res_ref[SIZE];
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = i * sign;
      s2.a[i] = (i + 20) * sign;
      sign = -sign;
      res3.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_or_si512) (s1.x, s2.x);
  res2.x = INTRINSIC (_or_epi32) (s1.x, s2.x);
  res3.x = INTRINSIC (_mask_or_epi32) (res3.x, mask, s1.x, s2.x);
  res4.x = INTRINSIC (_maskz_or_epi32) (mask, s1.x, s2.x);

  CALC (s1.a, s2.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_d) (res1, res_ref))
    abort ();

  if (UNION_CHECK (AVX512F_LEN, i_d) (res2, res_ref))
    abort ();

  MASK_MERGE (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res3, res_ref))
    abort ();

  MASK_ZERO (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res4, res_ref))
    abort ();
}
