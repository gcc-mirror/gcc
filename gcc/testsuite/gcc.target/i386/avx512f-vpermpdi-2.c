/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

#define N 0x7c

static void
CALC (double *s1, double *r)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      int index = (N >> ((i % 4) * 2)) & 3;
      int base = i / 4;
      r[i] = s1[4 * base + index];
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, d) src1, res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  double res_ref[SIZE];
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = i * i * sign;
      res2.a[i] = DEFAULT_VALUE;
      sign = -sign;
    }

  res1.x = INTRINSIC (_permutex_pd) (src1.x, N);
  res2.x = INTRINSIC (_mask_permutex_pd) (res2.x, mask, src1.x, N);
  res3.x = INTRINSIC (_maskz_permutex_pd) (mask, src1.x, N);

  CALC (src1.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, d) (res1, res_ref))
    abort ();

  MASK_MERGE (d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res2, res_ref))
    abort ();

  MASK_ZERO (d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res3, res_ref))
    abort ();
}
