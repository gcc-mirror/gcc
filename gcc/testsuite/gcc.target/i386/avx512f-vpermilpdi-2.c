/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

#ifndef CTRL
#define CTRL 129
#endif

static void
CALC (double *s1, int s2, double *r)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      r[i] = (s2 & (1 << i)) ? s1[1 + 2 * (i / 2)] : s1[2 * (i / 2)];
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, d) s1, res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  double res_ref[SIZE];
  int i;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = i + 10.;
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_permute_pd) (s1.x, CTRL);
  res2.x = INTRINSIC (_mask_permute_pd) (res2.x, mask, s1.x, CTRL);
  res3.x = INTRINSIC (_maskz_permute_pd) (mask, s1.x, CTRL);

  CALC (s1.a, CTRL, res_ref);

  if (UNION_CHECK (AVX512F_LEN, d) (res1, res_ref))
    abort ();

  MASK_MERGE (d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res2, res_ref))
    abort ();

  MASK_ZERO (d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res3, res_ref))
    abort ();
}
