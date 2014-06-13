/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

void static
CALC (double *s, double *r)
{
  int i;

  for (i = 0; i < SIZE/2; i++)
    {
      r[2 * i] = s[2 * i];
      r[2 * i + 1] = s[2 * i];
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, d) s, res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  double res_ref[SIZE];
  int i;

  for (i = 0; i < SIZE; i++)
    {
      s.a[i] = i * 123.2 + 32.6;
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_movedup_pd) (s.x);
  res2.x = INTRINSIC (_mask_movedup_pd) (res2.x, mask, s.x);
  res3.x = INTRINSIC (_maskz_movedup_pd) (mask, s.x);

  CALC (s.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, d) (res1, res_ref))
    abort ();

  MASK_MERGE (d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res2, res_ref))
    abort ();

  MASK_ZERO (d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res3, res_ref))
    abort ();
}
