/* { dg-do run } */
/* { dg-options "-O2 -mavx512dq" } */
/* { dg-require-effective-target avx512dq } */

#define AVX512DQ
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

void
CALC (long long *r, long long *s)
{
  int i;
  for (i = 0; i < SIZE; i++)
    {
      r[i] = s[i % 2];
    }
}

void
TEST (void)
{
  int i, sign;
  UNION_TYPE (AVX512F_LEN, i_q) res1, res2, res3;
  UNION_TYPE (128, i_q) src;
  MASK_TYPE mask = SIZE | 123;
  long long res_ref[SIZE];

  sign = -1;
  for (i = 0; i < 2; i++)
    {
      src.a[i] = 34 * i * sign;
      sign = sign * -1;
    }
  for (i = 0; i < SIZE; i++)
    res2.a[i] = DEFAULT_VALUE;

  res1.x = INTRINSIC (_broadcast_i64x2) (src.x);
  res2.x = INTRINSIC (_mask_broadcast_i64x2) (res2.x, mask, src.x);
  res3.x = INTRINSIC (_maskz_broadcast_i64x2) (mask, src.x);

  CALC (res_ref, src.a);

  if (UNION_CHECK (AVX512F_LEN, i_q) (res1, res_ref))
    abort ();

  MASK_MERGE (i_q) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res2, res_ref))
    abort ();

  MASK_ZERO (i_q) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res3, res_ref))
    abort ();
}
