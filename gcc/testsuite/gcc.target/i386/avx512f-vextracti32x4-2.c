/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"
#include "string.h"

static void
CALC (UNION_TYPE (AVX512F_LEN, i_d) s1, int *res_ref, int mask)
{
  memset (res_ref, 0, 16);
  memcpy (res_ref, s1.a + mask * 4, 16);
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_d) s1;
  union128i_d res1, res2, res3;
  int res_ref[4];
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

  res1.x = INTRINSIC (_extracti32x4_epi32) (s1.x, 1);
  res2.x =
    INTRINSIC (_mask_extracti32x4_epi32) (res2.x, mask, s1.x, 1);
  res3.x = INTRINSIC (_maskz_extracti32x4_epi32) (mask, s1.x, 1);
  CALC (s1, res_ref, 1);

  if (check_union128i_d (res1, res_ref))
    abort ();

  MASK_MERGE (i_d) (res_ref, mask, 4);
  if (check_union128i_d (res2, res_ref))
    abort ();

  MASK_ZERO (i_d) (res_ref, mask, 4);
  if (check_union128i_d (res3, res_ref))
    abort ();
}
