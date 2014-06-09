/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

static void
CALC (double *s1, double *s2, double *s3, double *r)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      r[i] = s1[i] * s2[i] + s3[i];
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, d) s1, s2, s3, res1, res2, res3, res4;
  MASK_TYPE mask = MASK_VALUE;
  double res_ref1[SIZE];
  double res_ref2[SIZE];
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = DEFAULT_VALUE;
      s2.a[i] = 56.78 * (i + 1) * sign;
      s3.a[i] = 90.12 * (i + 2) * sign;
      sign = -sign;
    }

#if AVX512F_LEN == 512
  res1.x = INTRINSIC (_fmadd_pd) (s1.x, s2.x, s3.x);
#endif
  res2.x = INTRINSIC (_mask_fmadd_pd) (s1.x, mask, s2.x, s3.x);
  res3.x = INTRINSIC (_mask3_fmadd_pd) (s2.x, s3.x, s1.x, mask);
  res4.x = INTRINSIC (_maskz_fmadd_pd) (mask, s1.x, s2.x, s3.x);

  CALC (s1.a, s2.a, s3.a, res_ref1);
  CALC (s2.a, s3.a, s1.a, res_ref2);

#if AVX512F_LEN == 512
  if (UNION_ROUGH_CHECK (AVX512F_LEN, d) (res1, res_ref1, 0.0001))
    abort ();
#endif

  MASK_MERGE (d) (res_ref1, mask, SIZE);
  if (UNION_ROUGH_CHECK (AVX512F_LEN, d) (res2, res_ref1, 0.0001))
    abort ();

  MASK_MERGE (d) (res_ref2, mask, SIZE);
  if (UNION_ROUGH_CHECK (AVX512F_LEN, d) (res3, res_ref2, 0.0001))
    abort ();

  MASK_ZERO (d) (res_ref1, mask, SIZE);
  if (UNION_ROUGH_CHECK (AVX512F_LEN, d) (res4, res_ref1, 0.0001))
    abort ();
}
