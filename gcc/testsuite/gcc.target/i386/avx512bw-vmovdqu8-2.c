/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SIZE ((AVX512F_LEN) / 8)
#include "avx512f-mask-type.h"

typedef struct
{
  char c;
  char a[SIZE];
} __attribute__ ((packed)) EVAL(unaligned_array, AVX512F_LEN,);

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_b) s1, s3, res1, res2, res3, res4;
  EVAL(unaligned_array, AVX512F_LEN,) s2, res5;
  MASK_TYPE mask = MASK_VALUE;
  int i, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = (i + 1) * sign;
      s2.a[i] = (i + 2) * sign;
      s3.a[i] = (i * 2) * sign;
      res1.a[i] = DEFAULT_VALUE;
      res3.a[i] = DEFAULT_VALUE;
      res5.a[i] = DEFAULT_VALUE;
      sign = -sign;
    }

  res1.x = INTRINSIC (_mask_mov_epi8) (res1.x, mask, s1.x);
  res2.x = INTRINSIC (_maskz_mov_epi8) (mask, s1.x);
  res3.x = INTRINSIC (_mask_loadu_epi8) (res3.x, mask, s2.a);
  res4.x = INTRINSIC (_maskz_loadu_epi8) (mask, s2.a);
  INTRINSIC (_mask_storeu_epi8) (res5.a, mask, s3.x);

  MASK_MERGE (i_b) (s1.a, mask, SIZE);
  if (checkVc (res1.a, s1.a, SIZE))
    abort ();

  MASK_ZERO (i_b) (s1.a, mask, SIZE);
  if (checkVc (res2.a, s1.a, SIZE))
    abort ();

  MASK_MERGE (i_b) (s2.a, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_b) (res3, s2.a))
    abort ();

  MASK_ZERO (i_b) (s2.a, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_b) (res4, s2.a))
    abort ();

  MASK_MERGE (i_b) (s3.a, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_b) (s3, res5.a))
    abort ();
}
