/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

#define N 0x5

static void
CALC (int *s1, int count, int *r)
{
  unsigned int i;

  for (i = 0; i < SIZE; i++)
    r[i] = (s1[i] << count) | (s1[i] >> (sizeof (s1[i]) * 8 - count));
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_d) s1, res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  int res_ref[SIZE];
  unsigned int i;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = 137 * i;
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_rol_epi32) (s1.x, N);
  res2.x = INTRINSIC (_mask_rol_epi32) (res2.x, mask, s1.x, N);
  res3.x = INTRINSIC (_maskz_rol_epi32) (mask, s1.x, N);

  CALC (s1.a, N, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_d) (res1, res_ref))
    abort ();

  MASK_MERGE (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res2, res_ref))
    abort ();

  MASK_ZERO (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res3, res_ref))
    abort ();
}
