/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SRC_SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"
#define DST_SIZE (AVX512F_LEN / 64)

static void
CALC (int *s1, int *s2, long long int *r)
{
  int i;

  for (i = 0; i < DST_SIZE; i++)
    r[i] = s1[i * 2] * s2[i * 2];
}

static void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_d) s1, s2;
  UNION_TYPE (AVX512F_LEN, i_q) res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  long long res_ref[DST_SIZE];
  int i, sign = 1;

  for (i = 0; i < SRC_SIZE; i++)
    {
      s1.a[i] = i * 20;
      s2.a[i] = i + 20;
    }

  for (i = 0; i < DST_SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  CALC (s1.a, s2.a, res_ref);

  res1.x = INTRINSIC (_mul_epi32) (s1.x, s2.x);
  res2.x = INTRINSIC (_mask_mul_epi32) (res2.x, mask, s1.x, s2.x);
  res3.x = INTRINSIC (_maskz_mul_epi32) (mask, s1.x, s2.x);

  if (UNION_CHECK (AVX512F_LEN, i_q) (res1, res_ref))
    abort ();

  MASK_MERGE (i_q) (res_ref, mask, DST_SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res2, res_ref))
    abort ();

  MASK_ZERO (i_q) (res_ref, mask, DST_SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res3, res_ref))
    abort ();
}
