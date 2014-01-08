/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE ((AVX512F_LEN) / 64)
#include "avx512f-mask-type.h"
#define ALIGN ((AVX512F_LEN) / 8)

static void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, d) s2, s3, res1, res3, res4, res5, res6;
  MASK_TYPE mask = MASK_VALUE;
  double s1[SIZE] __attribute__ ((aligned (ALIGN)));
  double res2[SIZE] __attribute__ ((aligned (ALIGN)));
  double res7[SIZE] __attribute__ ((aligned (ALIGN)));
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      s1[i] = 12.34 * (i + 2000) * sign;
      s2.a[i] = 56.78 * (i - 30) * sign;
      s3.a[i] = 90.12 * (i + 40) * sign;
      res3.a[i] = DEFAULT_VALUE;
      res5.a[i] = DEFAULT_VALUE;
      res7[i] = DEFAULT_VALUE;
      sign = -sign;
    }

#if AVX512F_LEN == 512
  res1.x = INTRINSIC (_load_pd) (s1);
  INTRINSIC (_store_pd) (res2, s2.x);
#endif
  res3.x = INTRINSIC (_mask_mov_pd) (res3.x, mask, s3.x);
  res4.x = INTRINSIC (_maskz_mov_pd) (mask, s3.x);
  res5.x = INTRINSIC (_mask_load_pd) (res5.x, mask, s1);
  res6.x = INTRINSIC (_maskz_load_pd) (mask, s1);
  INTRINSIC (_mask_store_pd) (res7, mask, s2.x);

#if AVX512F_LEN == 512
  if (UNION_CHECK (AVX512F_LEN, d) (res1, s1))
    abort ();

  if (UNION_CHECK (AVX512F_LEN, d) (s2, res2))
    abort ();
#endif

  MASK_MERGE (d) (s3.a, mask, SIZE);
  if (checkVd (res3.a, s3.a, SIZE))
    abort ();

  MASK_ZERO (d) (s3.a, mask, SIZE);
  if (checkVd (res4.a, s3.a, SIZE))
    abort ();

  MASK_MERGE (d) (s1, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res5, s1))
    abort ();

  MASK_ZERO (d) (s1, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res6, s1))
    abort ();

  MASK_MERGE (d) (s2.a, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (s2, res7))
    abort ();
}
