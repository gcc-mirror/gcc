/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

#include <string.h>

#define N 0x5

void
CALC (short *s1, short *r)
{
  int i;

  memset (r, 0, SIZE);

  if (N < 16)
    for (i = 0; i < SIZE; ++i)
      r[i] = s1[i] >> N;
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_w) s1, res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  union128i_q s2;
  short res_ref[SIZE];
  int i, sign;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = i * sign;
      sign = -sign;
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_srai_epi16) (s1.x, N);
  res2.x = INTRINSIC (_mask_srai_epi16) (res2.x, mask, s1.x, N);
  res3.x = INTRINSIC (_maskz_srai_epi16) (mask, s1.x, N);

  CALC (s1.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_w) (res1, res_ref))
    abort ();

  MASK_MERGE (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res2, res_ref))
    abort ();

  MASK_ZERO (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res3, res_ref))
    abort ();
}
