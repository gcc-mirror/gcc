/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

static void
CALC (long long *s1, long long *s2, long long *r)
{
  unsigned int i;

  for (i = 0; i < SIZE; i++)
    r[i] = (s1[i] << s2[i]) | (s1[i] >> (sizeof (s1[i]) * 8 - s2[i]));
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_q) s1, s2, res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  long long res_ref[SIZE];
  unsigned int i;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = 137 * i;
      s2.a[i] = (i + 7);
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_rolv_epi64) (s1.x, s2.x);
  res2.x = INTRINSIC (_mask_rolv_epi64) (res2.x, mask, s1.x, s2.x);
  res3.x = INTRINSIC (_maskz_rolv_epi64) (mask, s1.x, s2.x);

  CALC (s1.a, s2.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_q) (res1, res_ref))
    abort ();

  MASK_MERGE (i_q) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res2, res_ref))
    abort ();

  MASK_ZERO (i_q) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res3, res_ref))
    abort ();
}
