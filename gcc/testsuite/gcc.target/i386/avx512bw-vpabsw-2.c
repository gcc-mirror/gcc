/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

void
CALC (short *s, short *r)
{
  int i;

  for (i = 0; i < SIZE; i++)
    if (s[i] < 0)
      r[i] = -s[i];
    else
      r[i] = s[i];
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_w) s, res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  short res_ref[SIZE];
  int i;

  for (i = 0; i < SIZE; i++)
    {
      s.a[i] = i * 7 + (i << 15) + 356;
      res2.a[i] = DEFAULT_VALUE;
    }

  CALC (s.a, res_ref);

  res1.x = INTRINSIC (_abs_epi16) (s.x);
  res2.x = INTRINSIC (_mask_abs_epi16) (res2.x, mask, s.x);
  res3.x = INTRINSIC (_maskz_abs_epi16) (mask, s.x);

  if (UNION_CHECK (AVX512F_LEN, i_w) (res1, res_ref))
    abort ();
  MASK_MERGE (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res2, res_ref))
    abort ();

  MASK_ZERO (i_w) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res3, res_ref))
    abort ();
}
