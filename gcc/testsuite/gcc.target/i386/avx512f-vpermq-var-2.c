/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"


#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

static void
CALC (long long *mask, long long *src1, long long *dst)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
#if AVX512F_LEN == 512
      dst[i] = src1[mask[i] & 7];
#else
      dst[i] = src1[mask[i] & 3];
#endif
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_q) res1, res2, res3, src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  long long res_ref[SIZE];
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = (i + 10) * (i + 10) * sign;
      src2.a[i] = 2 * i + 10;
      sign = -sign;
      res3.a[i] = DEFAULT_VALUE;
    }

#if AVX512F_LEN == 512
  res1.x = INTRINSIC (_permutexvar_epi64) (src1.x, src2.x);
#endif
  res2.x = INTRINSIC (_maskz_permutexvar_epi64) (mask, src1.x, src2.x);
  res3.x = INTRINSIC (_mask_permutexvar_epi64) (res3.x, mask, src1.x, src2.x);

  CALC (src1.a, src2.a, res_ref);

#if AVX512F_LEN == 512
  if (UNION_CHECK (AVX512F_LEN, i_q) (res1, res_ref))
    abort ();
#endif

  MASK_ZERO (i_q) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res2, res_ref))
    abort ();

  MASK_MERGE (i_q) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res3, res_ref))
    abort ();
}
