/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define DST_SIZE (AVX512F_LEN / 16)
#define SRC_SIZE (AVX512F_LEN / 32)

#include "limits.h"

#include "avx512f-mask-type.h"

static short
EVAL(int_to_short, AVX512F_LEN,) (int iVal)
{
  short sVal;

  if (iVal < SHRT_MIN)
    sVal = SHRT_MIN;
  else if (iVal > SHRT_MAX)
    sVal = SHRT_MAX;
  else
    sVal = iVal;

  return sVal;
}

void
CALC (int *src1, int *src2, short *dst)
{
  int i;
  int *ptr;

  for (i = 0; i < DST_SIZE; i++)
    {
      ptr = (i / 4) % 2 ? src2 : src1;
      dst[i] = EVAL(int_to_short, AVX512F_LEN,) (ptr[i % 4 + (i / 8) * 4]);
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_d) s1, s2;
  UNION_TYPE (AVX512F_LEN, i_w) res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  short dst_ref[DST_SIZE];
  int i;

  for (i = 0; i < DST_SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  for (i = 0; i < SRC_SIZE; i++)
    {
      s1.a[i] = i + 10;
      s2.a[i] = i + 15;
    }

  res1.x = INTRINSIC (_packs_epi32) (s1.x, s2.x);
  res2.x = INTRINSIC (_mask_packs_epi32) (res2.x, mask, s1.x, s2.x);
  res3.x = INTRINSIC (_maskz_packs_epi32) (mask, s1.x, s2.x);

  CALC (s1.a, s2.a, dst_ref);

  if (UNION_CHECK (AVX512F_LEN, i_w) (res1, dst_ref))
    abort ();

  MASK_MERGE (i_w) (dst_ref, mask, DST_SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res2, dst_ref))
    abort ();

  MASK_ZERO (i_w) (dst_ref, mask, DST_SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res3, dst_ref))
    abort ();

}
