/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SRC_SIZE ((AVX512F_LEN) / 64)
#include "avx512f-mask-type.h"
#define DST_SIZE ((AVX512F_LEN_HALF) / 32)

static void
CALC (double *s, int *r)
{
  int i;

  for (i = 0; i < SRC_SIZE; i++)
    {
      r[i] = (s[i] >= 0) ? (int) (s[i] + 0.5)
			 : (int) (s[i] - 0.5);
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, d) s;
  UNION_TYPE (AVX512F_LEN_HALF, i_d) res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  int res_ref[DST_SIZE] = { 0 };
  int i, sign = 1;

  for (i = 0; i < SRC_SIZE; i++)
    {
      s.a[i] = 123.456 * (i + 2000) * sign;
      sign = -sign;
    }

  for (i = 0; i < DST_SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_cvtpd_epi32) (s.x);
  res2.x = INTRINSIC (_mask_cvtpd_epi32) (res2.x, mask, s.x);
  res3.x = INTRINSIC (_maskz_cvtpd_epi32) (mask, s.x);

  CALC (s.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN_HALF, i_d) (res1, res_ref))
    abort ();

  MASK_MERGE (i_d) (res_ref, mask, SRC_SIZE);
  if (UNION_CHECK (AVX512F_LEN_HALF, i_d) (res2, res_ref))
    abort ();

  MASK_ZERO (i_d) (res_ref, mask, SRC_SIZE);
  if (UNION_CHECK (AVX512F_LEN_HALF, i_d) (res3, res_ref))
    abort ();
}
