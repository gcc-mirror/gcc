/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"
#include "math.h"
#include "values.h"

static void
CALC (int *dst, int *src1, int *ind, int *src2)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      unsigned long long offset = ind[i] & (SIZE - 1);
      unsigned long long cond = ind[i] & SIZE;

      dst[i] = cond ? src2[offset] : src1[offset];
    }
}

void static
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_d) s1, s2, res1, res2, res3, ind;
  int res_ref[SIZE];

  MASK_TYPE mask = MASK_VALUE;

  for (i = 0; i < SIZE; i++)
    {
      ind.a[i] = 17 * (i << 1);
      s1.a[i] = DEFAULT_VALUE;
      s2.a[i] = 34 * i;

      res1.a[i] = DEFAULT_VALUE;
      res2.a[i] = DEFAULT_VALUE;
      res3.a[i] = DEFAULT_VALUE;
    }

  CALC (res_ref, s1.a, ind.a, s2.a);

  res1.x = INTRINSIC (_permutex2var_epi32) (s1.x, ind.x, s2.x);
  res2.x =
    INTRINSIC (_mask_permutex2var_epi32) (s1.x, mask, ind.x, s2.x);
  res3.x =
    INTRINSIC (_maskz_permutex2var_epi32) (mask, s1.x, ind.x, s2.x);

  if (UNION_CHECK (AVX512F_LEN, i_d) (res1, res_ref))
    abort ();

  MASK_MERGE (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res2, res_ref))
    abort ();

  MASK_ZERO (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res3, res_ref))
    abort ();
}
