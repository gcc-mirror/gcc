/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SRC_SIZE (AVX512F_LEN_HALF / 8)
#define DST_SIZE (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

void
CALC (char *s, short *r)
{
  int i;

  for (i = 0; i < DST_SIZE; i++)
    {
      r[i] = (short) s[i];
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN_HALF, i_b) s;
  UNION_TYPE (AVX512F_LEN, i_w) res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  short res_ref[DST_SIZE];
  int i, sign = 1;

  for (i = 0; i < SRC_SIZE; i++)
    {
      s.a[i] = 8 * i * sign;
      sign = -sign;
    }

  for (i = 0; i < DST_SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_cvtepi8_epi16) (s.x);
  res2.x = INTRINSIC (_mask_cvtepi8_epi16) (res2.x, mask, s.x);
  res3.x = INTRINSIC (_maskz_cvtepi8_epi16) (mask, s.x);

  CALC (s.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_w) (res1, res_ref))
    abort ();

  MASK_MERGE (i_w) (res_ref, mask, DST_SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res2, res_ref))
    abort ();

  MASK_ZERO (i_w) (res_ref, mask, DST_SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res3, res_ref))
    abort ();
}
