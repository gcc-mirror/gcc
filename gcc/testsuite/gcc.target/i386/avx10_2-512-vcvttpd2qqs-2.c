/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2-512" } */
/* { dg-require-effective-target avx10_2_512 } */

#ifndef AVX10_2
#define AVX10_2
#define AVX10_2_512
#define AVX10_512BIT
#endif
#include "avx10-helper.h"
#include <limits.h>

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

static void
CALC (double *s, long long *r)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      if (s[i] > LLONG_MAX)
	r[i] = LLONG_MAX;
      else if (s[i] < LLONG_MIN)
	r[i] = LLONG_MIN;
      else
	r[i] = s[i];
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, d) s;
  UNION_TYPE (AVX512F_LEN, i_q) res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  long long res_ref[SIZE] = { 0 };
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      s.a[i] = 1.23 * (i + 2) * sign;
      sign = -sign;
    }

  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

#if AVX512F_LEN == 128
  res1.x = INTRINSIC (_cvttspd_epi64) (s.x);
  res2.x = INTRINSIC (_mask_cvttspd_epi64) (res2.x, mask, s.x);
  res3.x = INTRINSIC (_maskz_cvttspd_epi64) (mask, s.x);
#else
  res1.x = INTRINSIC (_cvtts_roundpd_epi64) (s.x, 8);
  res2.x = INTRINSIC (_mask_cvtts_roundpd_epi64) (res2.x, mask, s.x, 8);
  res3.x = INTRINSIC (_maskz_cvtts_roundpd_epi64) (mask, s.x, 8);
#endif

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
