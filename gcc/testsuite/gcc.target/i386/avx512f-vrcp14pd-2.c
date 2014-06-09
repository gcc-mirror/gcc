/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

static void
CALC (double *s, double *r)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      r[i] = 1.0 / s[i];
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, d) s, res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  double res_ref[SIZE];
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      s.a[i] = 123.456 * (i + 2000) * sign;
      res2.a[i] = DEFAULT_VALUE;
      sign = -sign;
    }

  res1.x = INTRINSIC (_rcp14_pd) (s.x);
  res2.x = INTRINSIC (_mask_rcp14_pd) (res2.x, mask, s.x);
  res3.x = INTRINSIC (_maskz_rcp14_pd) (mask, s.x);

  CALC (s.a, res_ref);

  if (UNION_ROUGH_CHECK (AVX512F_LEN, d) (res1, res_ref, 0.0001))
    abort ();

  MASK_MERGE (d) (res_ref, mask, SIZE);
  if (UNION_ROUGH_CHECK (AVX512F_LEN, d) (res2, res_ref, 0.0001))
    abort ();

  MASK_ZERO (d) (res_ref, mask, SIZE);
  if (UNION_ROUGH_CHECK (AVX512F_LEN, d) (res3, res_ref, 0.0001))
    abort ();
}
