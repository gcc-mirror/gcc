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
CALC (short *s1, long long int *s2, short *r)
{
  int i;
  long long int count = s2[0];

  memset (r, 0, SIZE);

  if (count < 16)
    for (i = 0; i < SIZE; ++i)
      r[i] = s1[i] >> count;
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

  s2.a[0] = N;

  res1.x = INTRINSIC (_sra_epi16) (s1.x, s2.x);
  res2.x = INTRINSIC (_mask_sra_epi16) (res2.x, mask, s1.x, s2.x);
  res3.x = INTRINSIC (_maskz_sra_epi16) (mask, s1.x, s2.x);

  CALC (s1.a, s2.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_w) (res1, res_ref))
    abort ();

  MASK_MERGE (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res2, res_ref))
    abort ();

  MASK_ZERO (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res3, res_ref))
    abort ();
}
