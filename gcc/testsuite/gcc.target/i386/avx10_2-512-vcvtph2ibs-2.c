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

#define SIZE (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

static void
CALC (_Float16 *s, short *r)
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
	tmp = __builtin_nearbyintf16(s[i]);
      r[i] = (unsigned short) tmp;
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, h) s;
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

#if AVX512F_LEN == 128
  res1.x = INTRINSIC (_ipcvtph_epi16) (s.x);
  res2.x = INTRINSIC (_mask_ipcvtph_epi16) (res2.x, mask, s.x);
  res3.x = INTRINSIC (_maskz_ipcvtph_epi16) (mask, s.x);
#else
  res1.x = INTRINSIC (_ipcvt_roundph_epi16) (s.x, 8);
  res2.x = INTRINSIC (_mask_ipcvt_roundph_epi16) (res2.x, mask, s.x, 8);
  res3.x = INTRINSIC (_maskz_ipcvt_roundph_epi16) (mask, s.x, 8);
#endif

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
