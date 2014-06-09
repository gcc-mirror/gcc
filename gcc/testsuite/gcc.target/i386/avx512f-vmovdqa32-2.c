/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE ((AVX512F_LEN) / 32)
#include "avx512f-mask-type.h"
#define ALIGN ((AVX512F_LEN) / 8)

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_d) s2, s3, res1, res2, res5, res6, res7, res8;
  MASK_TYPE mask = MASK_VALUE;
  int s1[SIZE] __attribute__ ((aligned (ALIGN)));
  int res3[SIZE] __attribute__ ((aligned (ALIGN)));
  int res4[SIZE] __attribute__ ((aligned (ALIGN)));
  int res9[SIZE] __attribute__ ((aligned (ALIGN)));
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      s1[i] = 1234 * (i + 2000) * sign;
      s2.a[i] = 5678 * (i - 30) * sign;
      s3.a[i] = 9012 * (i + 40) * sign;
      res5.a[i] = DEFAULT_VALUE;
      res7.a[i] = DEFAULT_VALUE;
      res9[i] = DEFAULT_VALUE;
      sign = -sign;
    }

#if AVX512F_LEN == 512
  res1.x = INTRINSIC (_load_si512) (s1);
  res2.x = INTRINSIC (_load_epi32) (s1);
  INTRINSIC (_store_si512) (res3, s2.x);
  INTRINSIC (_store_epi32) (res4, s2.x);
#endif
  res5.x = INTRINSIC (_mask_mov_epi32) (res5.x, mask, s3.x);
  res6.x = INTRINSIC (_maskz_mov_epi32) (mask, s3.x);
  res7.x = INTRINSIC (_mask_load_epi32) (res7.x, mask, s1);
  res8.x = INTRINSIC (_maskz_load_epi32) (mask, s1);
  INTRINSIC (_mask_store_epi32) (res9, mask, s2.x);

#if AVX512F_LEN == 512
  if (UNION_CHECK (AVX512F_LEN, i_d) (res1, s1))
    abort ();

  if (UNION_CHECK (AVX512F_LEN, i_d) (res2, s1))
    abort ();

  if (UNION_CHECK (AVX512F_LEN, i_d) (s2, res3))
    abort ();

  if (UNION_CHECK (AVX512F_LEN, i_d) (s2, res4))
    abort ();
#endif

  MASK_MERGE (i_d) (s3.a, mask, SIZE);
  if (checkVi (res5.a, s3.a, SIZE))
    abort ();

  MASK_ZERO (i_d) (s3.a, mask, SIZE);
  if (checkVi (res6.a, s3.a, SIZE))
    abort ();

  MASK_MERGE (i_d) (s1, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res7, s1))
    abort ();

  MASK_ZERO (i_d) (s1, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res8, s1))
    abort ();

  MASK_MERGE (i_d) (s2.a, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (s2, res9))
    abort ();
}
