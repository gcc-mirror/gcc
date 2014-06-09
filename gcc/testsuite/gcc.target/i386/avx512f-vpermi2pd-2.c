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
CALC (double *dst, double *src1, long long *ind, double *src2)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      unsigned long long offset = ind[i] & (SIZE - 1);
      unsigned long long cond = ind[i] & SIZE;

      dst[i] = cond ? src2[offset] : src1[offset];
    }
}

void
TEST (void)
{
  int i, k;
  UNION_TYPE (AVX512F_LEN, d) s1, s2, res;
  UNION_TYPE (AVX512F_LEN, i_q) ind;
  double res_ref[SIZE];

  union
  {
    double f;
    long long i;
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

  res.x = INTRINSIC (_mask2_permutex2var_pd) (s1.x, ind.x, mask, s2.x);

  /* Standard MASK_MERGE cannot be used since VPERMI2PD in mask-merge mode
     merges vectors of two different types (_m512d and __m512i).  */
  for (k = 0; k < SIZE; k++)
    res_ref[k] = (mask & (1LL << k)) ? res_ref[k] : ind_copy[k].f;

  if (UNION_CHECK (AVX512F_LEN, d) (res, res_ref))
    abort ();
}
