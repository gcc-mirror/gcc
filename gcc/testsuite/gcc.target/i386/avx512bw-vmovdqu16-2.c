/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SIZE ((AVX512F_LEN) / 16)
#include "avx512f-mask-type.h"

typedef struct
{
  char c;
  short a[SIZE];
} __attribute__ ((packed)) EVAL(unaligned_array, AVX512F_LEN,);

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_w) s1, s3, res1, res2, res3, res4;
  EVAL(unaligned_array, AVX512F_LEN,) s2, res5;
  MASK_TYPE mask = MASK_VALUE;
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = 123 * i * sign;
      s2.a[i] = 456 * i * sign;
      s3.a[i] = 789 * i * sign;
      res1.a[i] = DEFAULT_VALUE;
      res3.a[i] = DEFAULT_VALUE;
      res5.a[i] = DEFAULT_VALUE;
      sign = -sign;
    }

  res1.x = INTRINSIC (_mask_mov_epi16) (res1.x, mask, s1.x);
  res2.x = INTRINSIC (_maskz_mov_epi16) (mask, s1.x);
  res3.x = INTRINSIC (_mask_loadu_epi16) (res3.x, mask, s2.a);
  res4.x = INTRINSIC (_maskz_loadu_epi16) (mask, s2.a);
  INTRINSIC (_mask_storeu_epi16) (res5.a, mask, s3.x);

  MASK_MERGE (i_w) (s1.a, mask, SIZE);
  if (checkVs (res1.a, s1.a, SIZE))
    abort ();

  MASK_ZERO (i_w) (s1.a, mask, SIZE);
  if (checkVs (res2.a, s1.a, SIZE))
    abort ();

  MASK_MERGE (i_w) (s2.a, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res3, s2.a))
    abort ();

  MASK_ZERO (i_w) (s2.a, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (res4, s2.a))
    abort ();

  MASK_MERGE (i_w) (s3.a, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_w) (s3, res5.a))
    abort ();
}
