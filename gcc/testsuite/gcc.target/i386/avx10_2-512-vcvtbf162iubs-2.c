/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2" } */
/* { dg-require-effective-target avx10_2 } */

#ifndef AVX10_2
#define AVX10_2
#define AVX10_2_512
#define AVX10_512BIT
#endif
#include "avx10-helper.h"
#include <math.h>
#include <limits.h>

#define SIZE (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

static void
CALC (__bf16 *s, unsigned short *r)
{
  int i;
  unsigned char tmp;

  for (i = 0; i < SIZE; i++)
    {
      if (s[i] > UCHAR_MAX)
	tmp = UCHAR_MAX;
      else if (s[i] < 0)
	tmp = 0;
      else
	tmp = nearbyint(_mm_cvtsbh_ss(s[i]));
      r[i] = (unsigned short)tmp;
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, bf16_bf) s;
  UNION_TYPE (AVX512F_LEN, i_uw) res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  unsigned short res_ref[SIZE] = { 0 };
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      s.a[i] = 1.23 * (i + 2) * sign;
      sign = -sign;
    }

  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_ipcvts_bf16_epu8) (s.x);
  res2.x = INTRINSIC (_mask_ipcvts_bf16_epu8) (res2.x, mask, s.x);
  res3.x = INTRINSIC (_maskz_ipcvts_bf16_epu8) (mask, s.x);

  CALC (s.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_uw) (res1, res_ref))
    abort ();

  MASK_MERGE (i_uw) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_uw) (res2, res_ref))
    abort ();

  MASK_ZERO (i_uw) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_uw) (res3, res_ref))
    abort ();
}
