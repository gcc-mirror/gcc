/* { dg-do run } */
/* { dg-options "-O2 -mavx512dq" } */
/* { dg-require-effective-target avx512dq } */

#define AVX512DQ
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"
#include "string.h"

void
CALC (long long int *s1, long long int *res_ref, int mask)
{
  memset (res_ref, 0, 16);
  memcpy (res_ref, s1 + mask * 2, 16);
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_q) s1;
  union128i_q res1, res2, res3;
  long long int res_ref[2];
  MASK_TYPE mask = MASK_VALUE;
  int j;

  for (j = 0; j < SIZE; j++)
    {
      s1.a[j] = j * j + 37;
    }

  for (j = 0; j < 2; j++)
    {
      res1.a[j] = DEFAULT_VALUE;
      res2.a[j] = DEFAULT_VALUE;
      res3.a[j] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_extracti64x2_epi64) (s1.x, 1);
  res2.x =
    INTRINSIC (_mask_extracti64x2_epi64) (res2.x, mask, s1.x, 1);
  res3.x = INTRINSIC (_maskz_extracti64x2_epi64) (mask, s1.x, 1);
  CALC (s1.a, res_ref, 1);

  if (check_union128i_q (res1, res_ref))
    abort ();

  MASK_MERGE (i_q) (res_ref, mask, 2);
  if (check_union128i_q (res2, res_ref))
    abort ();

  MASK_ZERO (i_q) (res_ref, mask, 2);
  if (check_union128i_q (res3, res_ref))
    abort ();
}
