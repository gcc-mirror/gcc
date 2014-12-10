/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

void
CALC (short *ind, short *src, short *res)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      res[i] = src[ind[i] & (SIZE - 1)];
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_w) s1, s2, res1, res2, res3;
  short res_ref[SIZE];
  MASK_TYPE mask = MASK_VALUE;
  int i;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = i * i * i;
      s2.a[i] = i + 20;
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_permutexvar_epi16) (s1.x, s2.x);
  res2.x = INTRINSIC (_mask_permutexvar_epi16) (res2.x, mask, s1.x, s2.x);
  res3.x = INTRINSIC (_maskz_permutexvar_epi16) (mask, s1.x, s2.x);
  CALC (s1.a, s2.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_w) (res1, res_ref))
    abort ();

  MASK_MERGE (i_w)(res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res2, res_ref))
    abort ();

  MASK_ZERO (i_w)(res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res3, res_ref))
    abort ();
}
