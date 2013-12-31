/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"
#include "math.h"
#include "values.h"

static void
CALC (long long *dst, long long *src1, long long *ind, long long *src2)
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
  UNION_TYPE (AVX512F_LEN, i_q) s1, s2, res, ind;
  long long res_ref[SIZE];

  MASK_TYPE mask = MASK_VALUE;

  for (i = 0; i < SIZE; i++)
    {
      ind.a[i] = DEFAULT_VALUE;
      s1.a[i] = 34 * i + 1;
      s2.a[i] = 34 * i;

      res.a[i] = DEFAULT_VALUE;
    }

  CALC (res_ref, s1.a, ind.a, s2.a);

  res.x =
    INTRINSIC (_mask2_permutex2var_epi64) (s1.x, ind.x, mask, s2.x);

  MASK_MERGE (i_q) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res, res_ref))
    abort ();
}
