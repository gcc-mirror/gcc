/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define DST_SIZE (AVX512F_LEN / 8)
#define SRC_SIZE (AVX512F_LEN / 16)

#include "limits.h"

#include "avx512f-mask-type.h"

static char
EVAL(short_to_char, AVX512F_LEN,) (short iVal)
{
  char sVal;

  if (iVal < CHAR_MIN)
    sVal = CHAR_MIN;
  else if (iVal > CHAR_MAX)
    sVal = CHAR_MAX;
  else
    sVal = iVal;

  return sVal;
}

void
CALC (short *src1, short *src2, char *dst)
{
  int i;
  short *ptr;

  for (i = 0; i < DST_SIZE; i++)
    {
      ptr = (i / 8) % 2 ? src2 : src1;
      dst[i] = EVAL(short_to_char, AVX512F_LEN,) (ptr[i % 8 + (i / 16) * 8]);
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_w) s1, s2;
  UNION_TYPE (AVX512F_LEN, i_b) res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  char dst_ref[DST_SIZE];
  int i;

  for (i = 0; i < DST_SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  for (i = 0; i < SRC_SIZE; i++)
    {
      s1.a[i] = i + 10;
      s2.a[i] = i + 15;
    }

  res1.x = INTRINSIC (_packs_epi16) (s1.x, s2.x);
  res2.x = INTRINSIC (_mask_packs_epi16) (res2.x, mask, s1.x, s2.x);
  res3.x = INTRINSIC (_maskz_packs_epi16) (mask, s1.x, s2.x);

  CALC (s1.a, s2.a, dst_ref);

  if (UNION_CHECK (AVX512F_LEN, i_b) (res1, dst_ref))
    abort ();

  MASK_MERGE (i_b) (dst_ref, mask, DST_SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_b) (res2, dst_ref))
    abort ();

  MASK_ZERO (i_b) (dst_ref, mask, DST_SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_b) (res3, dst_ref))
    abort ();

}
