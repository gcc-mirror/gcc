/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SRC_SIZE ((AVX512F_LEN_HALF) / 32)
#include "avx512f-mask-type.h"
#define DST_SIZE ((AVX512F_LEN) / 64)

static void
CALC (unsigned *s, double *r)
{
  int i;

  for (i = 0; i < DST_SIZE; i++)
    {
      r[i] = (double) s[i];
    }
}

static void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN_HALF, i_d) s;
  UNION_TYPE (AVX512F_LEN, d) res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  double res_ref[DST_SIZE];
  int i;

  for (i = 0; i < SRC_SIZE; i++)
    {
      s.a[i] = 123456 * (i + 2000);
    }

  for (i = 0; i < DST_SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_cvtepu32_pd) (s.x);
  res2.x = INTRINSIC (_mask_cvtepu32_pd) (res2.x, mask, s.x);
  res3.x = INTRINSIC (_maskz_cvtepu32_pd) (mask, s.x);

  CALC (s.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, d) (res1, res_ref))
    abort ();

  MASK_MERGE (d) (res_ref, mask, DST_SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res2, res_ref))
    abort ();

  MASK_ZERO (d) (res_ref, mask, DST_SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res3, res_ref))
    abort ();
}
