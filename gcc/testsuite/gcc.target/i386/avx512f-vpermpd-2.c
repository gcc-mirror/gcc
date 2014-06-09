/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

static void
CALC (long long *mask, double *s1, double *r)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      r[i] = s1[mask[i] & 7 % SIZE];
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, d) src1, res1, res2, res3;
  UNION_TYPE (AVX512F_LEN, i_q) src2;
  MASK_TYPE mask = MASK_VALUE;
  double res_ref[SIZE];
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = i * sign;
      src2.a[i] = i + 20;
      sign = -sign;
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_permutexvar_pd) (src2.x, src1.x);
  res2.x = INTRINSIC (_mask_permutexvar_pd) (res2.x, mask, src2.x, src1.x);
  res3.x = INTRINSIC (_maskz_permutexvar_pd) (mask, src2.x, src1.x);

  CALC (src2.a, src1.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, d) (res1, res_ref))
    abort ();

  MASK_MERGE (d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res2, res_ref))
    abort ();

  MASK_ZERO (d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res3, res_ref))
    abort ();
}
