/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

static void
CALC (char *s, long long int *r)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      r[i] = (long long int) s[i];
    }
}

static void
TEST (void)
{
  union128i_b s;
  UNION_TYPE (AVX512F_LEN, i_q) res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  long long int res_ref[SIZE];
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      s.a[i] = 8 * i * sign;
      res2.a[i] = DEFAULT_VALUE;
      sign = -sign;
    }

  res1.x = INTRINSIC (_cvtepi8_epi64) (s.x);
  res2.x = INTRINSIC (_mask_cvtepi8_epi64) (res2.x, mask, s.x);
  res3.x = INTRINSIC (_maskz_cvtepi8_epi64) (mask, s.x);

  CALC (s.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_q) (res1, res_ref))
    abort ();

  MASK_MERGE (i_q) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res2, res_ref))
    abort ();

  MASK_ZERO (i_q) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res3, res_ref))
    abort ();
}
