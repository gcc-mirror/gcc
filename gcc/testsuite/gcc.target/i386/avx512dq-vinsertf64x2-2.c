/* { dg-do run } */
/* { dg-options "-O2 -mavx512dq" } */
/* { dg-require-effective-target avx512dq } */

#define AVX512DQ
#include "avx512f-helper.h"
#include "string.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

void
CALC (UNION_TYPE (AVX512F_LEN, d) s1, union128d s2,
      double *res_ref, int mask)
{
  memcpy (res_ref, s1.a, SIZE * sizeof (double));
  memcpy (res_ref + mask * 2, s2.a, 16);
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, d) s1, res1, res2, res3;
  union128d s2;
  double res_ref[SIZE];
  MASK_TYPE mask = MASK_VALUE;
  int j;

  for (j = 0; j < SIZE; j++)
    {
      s1.a[j] = j * j;
      res1.a[j] = DEFAULT_VALUE;
      res2.a[j] = DEFAULT_VALUE;
      res3.a[j] = DEFAULT_VALUE;
    }

  for (j = 0; j < 2; j++)
    s2.a[j] = j * j * j;

  res1.x = INTRINSIC (_insertf64x2) (s1.x, s2.x, 1);
  res2.x = INTRINSIC (_mask_insertf64x2) (res2.x, mask, s1.x, s2.x, 1);
  res3.x = INTRINSIC (_maskz_insertf64x2) (mask, s1.x, s2.x, 1);

  CALC (s1, s2, res_ref, 1);

  if (UNION_CHECK (AVX512F_LEN, d) (res1, res_ref))
    abort ();

  MASK_MERGE (d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res2, res_ref))
    abort ();

  MASK_ZERO (d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res3, res_ref))
    abort ();
}
