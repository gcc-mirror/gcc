/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

void static
CALC (double *s1, double *s2, double *r)
{
  int i;

  for (i = 0; i < SIZE / 2; i++)
    {
      r[2 * i] = s1[2 * i];
      r[2 * i + 1] = s2[2 * i];
    }
}

void static
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, d) s1, s2, res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  double res_ref[SIZE];
  int i;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = i * 123.2 + 32.6;
      s2.a[i] = i + 2.5;
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_unpacklo_pd) (s1.x, s2.x);
  res2.x = INTRINSIC (_mask_unpacklo_pd) (res2.x, mask, s1.x, s2.x);
  res3.x = INTRINSIC (_maskz_unpacklo_pd) (mask, s1.x, s2.x);

  CALC (s1.a, s2.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, d) (res1, res_ref))
    abort ();

  MASK_MERGE (d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res2, res_ref))
    abort ();

  MASK_ZERO (d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res3, res_ref))
    abort ();
}
