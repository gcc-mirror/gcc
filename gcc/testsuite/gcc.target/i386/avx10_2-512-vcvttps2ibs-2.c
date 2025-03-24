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
#include <math.h>
#include <string.h>

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

static void
CALC (float *s, int *r)
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
	tmp = s[i];
      r[i] = (unsigned int)tmp;
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, ) s;
  UNION_TYPE (AVX512F_LEN, i_d) res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  int res_ref[SIZE] = { 0 }, res_ref2[SIZE] = { 0 };
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      s.a[i] = 1.23 * (i + 2) * sign;
      sign = -sign;
    }

  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_ipcvtts_ps_epi8) (s.x);
  res2.x = INTRINSIC (_mask_ipcvtts_ps_epi8) (res2.x, mask, s.x);
  res3.x = INTRINSIC (_maskz_ipcvtts_ps_epi8) (mask, s.x);

  CALC (s.a, res_ref);
  memcpy(res_ref2, res_ref, sizeof(res_ref));

  if (UNION_CHECK (AVX512F_LEN, i_d) (res1, res_ref))
    abort ();

  MASK_MERGE (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res2, res_ref))
    abort ();

  MASK_ZERO (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res3, res_ref))
    abort ();

#if AVX512F_LEN == 512
  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_ipcvtts_roundps_epi8) (s.x, 8);
  res2.x = INTRINSIC (_mask_ipcvtts_roundps_epi8) (res2.x, mask, s.x, 8);
  res3.x = INTRINSIC (_maskz_ipcvtts_roundps_epi8) (mask, s.x, 8);

  if (UNION_CHECK (AVX512F_LEN, i_d) (res1, res_ref2))
    abort ();

  MASK_MERGE (i_d) (res_ref2, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res2, res_ref2))
    abort ();

  MASK_ZERO (i_d) (res_ref2, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res3, res_ref2))
    abort ();
#endif
}
