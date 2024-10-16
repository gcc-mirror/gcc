/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2-512" } */
/* { dg-require-effective-target avx10_2_512 } */

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
CALC (__bf16 *s, short *r)
{
  int i;
  unsigned char tmp;

  for (i = 0; i < SIZE; i++)
    {
      if (s[i] > SCHAR_MAX)
	tmp = SCHAR_MAX;
      else if (s[i] < SCHAR_MIN)
	tmp = SCHAR_MIN;
      else
	tmp = nearbyint(_mm_cvtsbh_ss(s[i]));
      r[i] = (unsigned short)tmp;
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, bf16_bf) s;
  UNION_TYPE (AVX512F_LEN, i_w) res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  short res_ref[SIZE] = { 0 };
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      s.a[i] = 1.23 * (i + 2) * sign;
      sign = -sign;
    }

  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_ipcvtnebf16_epi16) (s.x);
  res2.x = INTRINSIC (_mask_ipcvtnebf16_epi16) (res2.x, mask, s.x);
  res3.x = INTRINSIC (_maskz_ipcvtnebf16_epi16) (mask, s.x);

  CALC (s.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_w) (res1, res_ref))
    abort ();

  MASK_MERGE (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res2, res_ref))
    abort ();

  MASK_ZERO (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res3, res_ref))
    abort ();
}
