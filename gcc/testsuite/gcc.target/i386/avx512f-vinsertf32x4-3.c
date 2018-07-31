/* { dg-do run } */
/* { dg-options "-O0 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"
#include "string.h"

void static
CALC (UNION_TYPE (AVX512F_LEN,) s1, union128 s2, float *res_ref, int imm)
{
  memcpy (res_ref, s1.a, SIZE * sizeof (float));
  memcpy (res_ref + imm * 4, s2.a, 16);
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN,) s1, res1, res2, res3;
  union128 s2;
  float res_ref[SIZE];
  int j;

  MASK_TYPE mask = (MASK_TYPE) 0xa55a;

  for (j = 0; j < SIZE; j++)
    {
      s1.a[j] = j * j / 10.2;
      res1.a[j] = DEFAULT_VALUE;
      res2.a[j] = DEFAULT_VALUE;
      res3.a[j] = DEFAULT_VALUE;
    }

  for (j = 0; j < 4; j++)
    s2.a[j] = j * j * j / 2.03;

  res1.x = INTRINSIC (_insertf32x4) (s1.x, s2.x, 1);
  res2.x = INTRINSIC (_mask_insertf32x4) (res2.x, mask, s1.x, s2.x, 1);
  res3.x = INTRINSIC (_maskz_insertf32x4) (mask, s1.x, s2.x, 1);

  CALC (s1, s2, res_ref, 1);

  if (UNION_CHECK (AVX512F_LEN,) (res1, res_ref))
    abort ();

  MASK_MERGE () (res_ref, mask, SIZE);

  if (UNION_CHECK (AVX512F_LEN,) (res2, res_ref))
    abort ();

  MASK_ZERO () (res_ref, mask, SIZE);

  if (UNION_CHECK (AVX512F_LEN,) (res3, res_ref))
    abort ();
}
