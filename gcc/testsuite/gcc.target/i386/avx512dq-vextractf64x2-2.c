/* { dg-do run } */
/* { dg-options "-O2 -mavx512dq" } */
/* { dg-require-effective-target avx512dq } */

#define AVX512DQ
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"
#include "string.h"

void
CALC (double *s1, double *res_ref, int mask)
{
  memset (res_ref, 0, 16);
  memcpy (res_ref, s1 + mask * 2, 16);
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, d) s1;
  union128d res1, res2, res3;
  double res_ref[2];
  MASK_TYPE mask = MASK_VALUE;
  int j;

  for (j = 0; j < SIZE; j++)
    {
      s1.a[j] = j * j / 4.56;
    }

  for (j = 0; j < 2; j++)
    {
      res1.a[j] = DEFAULT_VALUE;
      res2.a[j] = DEFAULT_VALUE;
      res3.a[j] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_extractf64x2_pd) (s1.x, 1);
  res2.x = INTRINSIC (_mask_extractf64x2_pd) (res2.x, mask, s1.x, 1);
  res3.x = INTRINSIC (_maskz_extractf64x2_pd) (mask, s1.x, 1);
  CALC (s1.a, res_ref, 1);

  if (check_union128d (res1, res_ref))
    abort ();

  MASK_MERGE (d) (res_ref, mask, 2);
  if (check_union128d (res2, res_ref))
    abort ();

  MASK_ZERO (d) (res_ref, mask, 2);
  if (check_union128d (res3, res_ref))
    abort ();
}
