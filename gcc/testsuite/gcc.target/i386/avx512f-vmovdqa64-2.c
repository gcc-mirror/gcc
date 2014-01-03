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
  UNION_TYPE (AVX512F_LEN, i_q) s2, s3, res1, res3, res4, res5, res6;
  MASK_TYPE mask = MASK_VALUE;
  long long s1[SIZE] __attribute__ ((aligned (ALIGN)));
  long long res2[SIZE] __attribute__ ((aligned (ALIGN)));
  long long res7[SIZE] __attribute__ ((aligned (ALIGN)));
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      s1[i] = 1234 * (i + 2000) * sign;
      s2.a[i] = 5678 * (i - 30) * sign;
      s3.a[i] = 9012 * (i + 40) * sign;
      res3.a[i] = DEFAULT_VALUE;
      res5.a[i] = DEFAULT_VALUE;
      res7[i] = DEFAULT_VALUE;
      sign = -sign;
    }

#if AVX512F_LEN == 512
  res1.x = INTRINSIC (_load_epi64) (s1);
  INTRINSIC (_store_epi64) (res2, s2.x);
#endif
  res3.x = INTRINSIC (_mask_mov_epi64) (res3.x, mask, s3.x);
  res4.x = INTRINSIC (_maskz_mov_epi64) (mask, s3.x);
  res5.x = INTRINSIC (_mask_load_epi64) (res5.x, mask, s1);
  res6.x = INTRINSIC (_maskz_load_epi64) (mask, s1);
  INTRINSIC (_mask_store_epi64) (res7, mask, s2.x);

#if AVX512F_LEN == 512
  if (UNION_CHECK (AVX512F_LEN, i_q) (res1, s1))
    abort ();

  if (UNION_CHECK (AVX512F_LEN, i_q) (s2, res2))
    abort ();
#endif

  MASK_MERGE (i_q) (s3.a, mask, SIZE);
  if (checkVl (res3.a, s3.a, SIZE))
    abort ();

  MASK_ZERO (i_q) (s3.a, mask, SIZE);
  if (checkVl (res4.a, s3.a, SIZE))
    abort ();

  MASK_MERGE (i_q) (s1, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res5, s1))
    abort ();

  MASK_ZERO (i_q) (s1, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res6, s1))
    abort ();

  MASK_MERGE (i_q) (s2.a, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (s2, res7))
    abort ();
}
