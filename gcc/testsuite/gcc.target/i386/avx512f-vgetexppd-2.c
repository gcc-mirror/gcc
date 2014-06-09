/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"
#include "math.h"

static void
CALC (double *s, double *r)
{
  int i = 0;
  for (i = 0; i < SIZE; i++)
    r[i] = floor (log (s[i]) / log (2));
}

void
TEST (void)
{
  int j;
  UNION_TYPE (AVX512F_LEN, d) res1, res2, res3, s;
  double res_ref[SIZE];

  MASK_TYPE mask = MASK_VALUE;

  for (j = 0; j < SIZE; j++)
    {
      s.a[j] = j * (j + 12.0231);
      res1.a[j] = DEFAULT_VALUE;
      res2.a[j] = DEFAULT_VALUE;
      res3.a[j] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_getexp_pd) (s.x);
  res2.x = INTRINSIC (_mask_getexp_pd) (res2.x, mask, s.x);
  res3.x = INTRINSIC (_maskz_getexp_pd) (mask, s.x);
  CALC (s.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, d) (res1, res_ref))
    abort ();

  MASK_MERGE(d) (res_ref, mask, SIZE);

  if (UNION_CHECK (AVX512F_LEN, d) (res2, res_ref))
    abort ();

  MASK_ZERO(d) (res_ref, mask, SIZE);

  if (UNION_CHECK (AVX512F_LEN, d) (res3, res_ref))
    abort ();

}

