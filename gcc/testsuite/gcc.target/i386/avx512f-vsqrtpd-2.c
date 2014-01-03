/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#include <math.h>
#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

static void
CALC (double *s, double *r)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      r[i] = sqrt(s[i]);
    }
}

static void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, d) s, res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  double res_ref[SIZE];
  int i;

  for (i = 0; i < SIZE; i++)
    {
      s.a[i] = 123.456 * (i + 2000);
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_sqrt_pd) (s.x);
  res2.x = INTRINSIC (_mask_sqrt_pd) (res2.x, mask, s.x);
  res3.x = INTRINSIC (_maskz_sqrt_pd) (mask, s.x);

  CALC (s.a, res_ref);

  if (UNION_FP_CHECK (AVX512F_LEN, d) (res1, res_ref))
    abort ();

  MASK_MERGE (d) (res_ref, mask, SIZE);
  if (UNION_FP_CHECK (AVX512F_LEN, d) (res2, res_ref))
    abort ();

  MASK_ZERO (d) (res_ref, mask, SIZE);
  if (UNION_FP_CHECK (AVX512F_LEN, d) (res3, res_ref))
    abort ();
}
