/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

static void
CALC (int *s1, unsigned char imm, int *r)
{
  int i, j, offset;

  for (j = 0; j < SIZE / 4; j++)
    {
      offset = j * 4;
      for (i = 0; i < 4; i++)
	r[i + offset] =
	  s1[((imm & (0x3 << (2 * i))) >> (2 * i)) + offset];
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_d) s1, res1, res2, res3;
  int res_ref[SIZE];
  int j, sign = 1;
  MASK_TYPE mask = MASK_VALUE;

  for (j = 0; j < SIZE; j++)
    {
      s1.a[j] = j * sign;
      res1.a[j] = DEFAULT_VALUE;
      res2.a[j] = DEFAULT_VALUE;
      res3.a[j] = DEFAULT_VALUE;
      sign = -sign;
    }

  res1.x = INTRINSIC (_shuffle_epi32) (s1.x, 0xec);
  res2.x = INTRINSIC (_mask_shuffle_epi32) (res2.x, mask, s1.x, 0xec);
  res3.x = INTRINSIC (_maskz_shuffle_epi32) (mask, s1.x, 0xec);

  CALC (s1.a, 0xec, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_d) (res1, res_ref))
    abort ();

  MASK_MERGE (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res2, res_ref))
    abort ();

  MASK_ZERO (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res3, res_ref))
    abort ();
}
