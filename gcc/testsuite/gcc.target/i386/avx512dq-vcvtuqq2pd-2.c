/* { dg-do run } */
/* { dg-options "-O2 -mavx512dq" } */
/* { dg-require-effective-target avx512dq } */

#define AVX512DQ
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

void
CALC (unsigned long long *s, double *r)
{
  int i;

  for (i = 0; i < SIZE; i++)
    r[i] = (double) s[i];
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_q) s;
  UNION_TYPE (AVX512F_LEN, d) res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  double res_ref[SIZE];
  int i;

  for (i = 0; i < SIZE; i++)
    {
      s.a[i] = 123 * (i + 2000);
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_cvtepu64_pd) (s.x);
  res2.x = INTRINSIC (_mask_cvtepu64_pd) (res2.x, mask, s.x);
  res3.x = INTRINSIC (_maskz_cvtepu64_pd) (mask, s.x);

  CALC (s.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, d) (res1, res_ref))
    abort ();

  MASK_MERGE (d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res2, res_ref))
    abort ();

  MASK_ZERO (d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res3, res_ref))
    abort ();
}
