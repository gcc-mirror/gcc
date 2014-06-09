/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"
#include "string.h"

static void
CALC (UNION_TYPE (AVX512F_LEN,) s1, float *res_ref, int mask)
{
  memset (res_ref, 0, 16);
  memcpy (res_ref, s1.a + mask * 4, 16);
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN,) s1;
  union128 res1, res2, res3;
  float res_ref[4];
  MASK_TYPE mask = MASK_VALUE;
  int j;

  for (j = 0; j < SIZE; j++)
    {
      s1.a[j] = j * j / 4.56;
    }

  for (j = 0; j < 4; j++)
    {
      res1.a[j] = DEFAULT_VALUE;
      res2.a[j] = DEFAULT_VALUE;
      res3.a[j] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_extractf32x4_ps) (s1.x, 1);
  res2.x = INTRINSIC (_mask_extractf32x4_ps) (res2.x, mask, s1.x, 1);
  res3.x = INTRINSIC (_maskz_extractf32x4_ps) (mask, s1.x, 1);
  CALC (s1, res_ref, 1);

  if (check_union128 (res1, res_ref))
    abort ();

  MASK_MERGE ()(res_ref, mask, 4);
  if (check_union128 (res2, res_ref))
    abort ();

  MASK_ZERO ()(res_ref, mask, 4);
  if (check_union128 (res3, res_ref))
    abort ();
}
