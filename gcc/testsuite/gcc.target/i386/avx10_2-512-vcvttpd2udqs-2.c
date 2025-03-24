/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2" } */
/* { dg-require-effective-target avx10_2 } */

#ifndef AVX10_2
#define AVX10_2
#define AVX10_2_512
#define AVX10_512BIT
#endif
#include "avx10-helper.h"
#include <limits.h>
#include <string.h>

#define SRC_SIZE (AVX512F_LEN / 64)
#define SIZE (AVX512F_LEN_HALF / 32)
#include "avx512f-mask-type.h"

static void
CALC (double *s, unsigned int *r)
{
  int i;

  for (i = 0; i < SRC_SIZE; i++)
    {
      if (s[i] > UINT_MAX)
	r[i] = UINT_MAX;
      else if (s[i] < 0)
	r[i] = 0;
      else
	r[i] = s[i];
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, d) s;
  UNION_TYPE (AVX512F_LEN_HALF, i_ud) res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  unsigned int res_ref[SIZE] = { 0 }, res_ref2[SIZE] = { 0 };
  int i, sign = 1;

  for (i = 0; i < SRC_SIZE; i++)
    {
      s.a[i] = 1.23 * (i + 2) * sign;
      sign = -sign;
    }

  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_cvtts_pd_epu32) (s.x);
  res2.x = INTRINSIC (_mask_cvtts_pd_epu32) (res2.x, mask, s.x);
  res3.x = INTRINSIC (_maskz_cvtts_pd_epu32) (mask, s.x);

  CALC (s.a, res_ref);
  memcpy(res_ref2, res_ref, sizeof(res_ref));

  if (UNION_CHECK (AVX512F_LEN_HALF, i_ud) (res1, res_ref))
    abort ();

  MASK_MERGE (i_ud) (res_ref, mask, SRC_SIZE);
  if (UNION_CHECK (AVX512F_LEN_HALF, i_ud) (res2, res_ref))
    abort ();

  MASK_ZERO (i_ud) (res_ref, mask, SRC_SIZE);
  if (UNION_CHECK (AVX512F_LEN_HALF, i_ud) (res3, res_ref))
    abort ();

#if AVX512F_LEN == 512
  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_cvtts_roundpd_epu32) (s.x, 8);
  res2.x = INTRINSIC (_mask_cvtts_roundpd_epu32) (res2.x, mask, s.x, 8);
  res3.x = INTRINSIC (_maskz_cvtts_roundpd_epu32) (mask, s.x, 8);

  if (UNION_CHECK (AVX512F_LEN_HALF, i_ud) (res1, res_ref2))
    abort ();

  MASK_MERGE (i_ud) (res_ref2, mask, SRC_SIZE);
  if (UNION_CHECK (AVX512F_LEN_HALF, i_ud) (res2, res_ref2))
    abort ();

  MASK_ZERO (i_ud) (res_ref2, mask, SRC_SIZE);
  if (UNION_CHECK (AVX512F_LEN_HALF, i_ud) (res3, res_ref2))
    abort ();
#endif
}
