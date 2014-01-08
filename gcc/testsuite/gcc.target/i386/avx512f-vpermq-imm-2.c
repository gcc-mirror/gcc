/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"


#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

#define IMM_MASK 0x7c

static void
CALC (long long *src1, int mask, long long *dst)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      int index = ((mask >> (2 * (i % 4))) & 3);
      int base = i / 4;
      dst[i] = src1[4 * base + index];
    }
}

static void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_q) res1, res2, res3, src1;
  MASK_TYPE mask = MASK_VALUE;
  long long res_ref[SIZE];
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = (i + 10) * (i + 10) * sign;
      sign = -sign;
      res3.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_permutex_epi64) (src1.x, IMM_MASK);
  res2.x = INTRINSIC (_maskz_permutex_epi64) (mask, src1.x, IMM_MASK);
  res3.x = INTRINSIC (_mask_permutex_epi64) (res3.x, mask, src1.x, IMM_MASK);

  CALC (src1.a, IMM_MASK, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_q) (res1, res_ref))
    abort ();

  MASK_ZERO (i_q) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res2, res_ref))
    abort ();

  MASK_MERGE (i_q) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res3, res_ref))
    abort ();
}
