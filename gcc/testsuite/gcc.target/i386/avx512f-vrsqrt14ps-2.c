/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#include <math.h>
#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

static void
CALC (float *s, float *r)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      r[i] = 1.0 / sqrt(s[i]);
    }
}

static void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN,) s, res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  float res_ref[SIZE];
  int i;

  for (i = 0; i < SIZE; i++)
    {
      s.a[i] = 123.456 * (i + 2000);
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_rsqrt14_ps) (s.x);
  res2.x = INTRINSIC (_mask_rsqrt14_ps) (res2.x, mask, s.x);
  res3.x = INTRINSIC (_maskz_rsqrt14_ps) (mask, s.x);

  CALC (s.a, res_ref);

  if (UNION_ROUGH_CHECK (AVX512F_LEN,) (res1, res_ref, 0.0001))
    abort ();

  MASK_MERGE () (res_ref, mask, SIZE);
  if (UNION_ROUGH_CHECK (AVX512F_LEN,) (res2, res_ref, 0.0001))
    abort ();

  MASK_ZERO () (res_ref, mask, SIZE);
  if (UNION_ROUGH_CHECK (AVX512F_LEN,) (res3, res_ref, 0.0001))
    abort ();
}
