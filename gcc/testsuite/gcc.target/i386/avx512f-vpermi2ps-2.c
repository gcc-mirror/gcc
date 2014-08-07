/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"
#include "math.h"

static void
CALC (float *dst, float *src1, int *ind, float *src2)
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
  int i, k;
  UNION_TYPE (AVX512F_LEN,) s1, s2, res;
  UNION_TYPE (AVX512F_LEN, i_d) ind;
  float res_ref[SIZE];

  union
  {
    float f;
    int i;
  } ind_copy[SIZE];

  MASK_TYPE mask = MASK_VALUE;

  for (i = 0; i < SIZE; i++)
    {
      /* Some of the integer indexes may be interpreted as floating point
         values in mask-merge mode, that's why we use IND_COPY.  */
      ind.a[i] = ind_copy[i].i = 17 * (i << 1);
      s1.a[i] = 42.5 * i + 1;
      s2.a[i] = 22.5 * i;

      res.a[i] = DEFAULT_VALUE;
    }

  CALC (res_ref, s1.a, ind.a, s2.a);

  res.x = INTRINSIC (_mask2_permutex2var_ps) (s1.x, ind.x, mask, s2.x);

  /* Standard MASK_MERGE cannot be used since VPERMI2PS in mask-merge mode
     merges vectors of two different types (_m512 and __m512i).  */
  for (k = 0; k < SIZE; k++)
    res_ref[k] = (mask & (1LL << k)) ? res_ref[k] : ind_copy[k].f;

  if (UNION_CHECK (AVX512F_LEN,) (res, res_ref))
    abort ();
}
