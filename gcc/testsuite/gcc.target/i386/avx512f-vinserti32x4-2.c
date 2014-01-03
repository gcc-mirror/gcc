/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"
#include "string.h"

void static
CALC (UNION_TYPE (AVX512F_LEN, i_d) s1, union128i_d s2, int *res_ref, int imm)
{
  memcpy (res_ref, s1.a, SIZE * sizeof (int));
  memcpy (res_ref + imm * 4, s2.a, 16);
}

void static
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_d) s1, res1, res2, res3;
  union128i_d s2;
  int res_ref[SIZE];
  int j;

  MASK_TYPE mask = 6 ^ (0xffd >> SIZE);

  for (j = 0; j < SIZE; j++)
    {
      s1.a[j] = j * j;
      res1.a[j] = DEFAULT_VALUE;
      res2.a[j] = DEFAULT_VALUE;
      res3.a[j] = DEFAULT_VALUE;
    }

  for (j = 0; j < 4; j++)
    s2.a[j] = j * j * j;

  res1.x = INTRINSIC (_inserti32x4) (s1.x, s2.x, 1);
  res2.x = INTRINSIC (_mask_inserti32x4) (res2.x, mask, s1.x, s2.x, 1);
  res3.x = INTRINSIC (_maskz_inserti32x4) (mask, s1.x, s2.x, 1);

  CALC (s1, s2, res_ref, 1);

  if (UNION_CHECK (AVX512F_LEN, i_d) (res1, res_ref))
    abort ();

  MASK_MERGE (i_d) (res_ref, mask, SIZE);

  if (UNION_CHECK (AVX512F_LEN, i_d) (res2, res_ref))
    abort ();

  MASK_ZERO (i_d) (res_ref, mask, SIZE);

  if (UNION_CHECK (AVX512F_LEN, i_d) (res3, res_ref))
    abort ();
}
