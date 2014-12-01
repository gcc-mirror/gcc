/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

#define AVX512BW
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 8)
#include "avx512f-mask-type.h"

void
CALC (char *s1, char *s2, char *r)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      if (s2[i] < 0)
	r[i] = 0;
      else
	r[i] = s1[(s2[i] & 0xf) + 16 * (i / 16)];
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_b) s1, s2, res1, res2, res3;
  char res_ref[SIZE];
  int i, sign = 1;
  MASK_TYPE mask = MASK_VALUE;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = i * i * sign;
      s2.a[i] = 179 - i;
      res1.a[i] = DEFAULT_VALUE;
      res2.a[i] = DEFAULT_VALUE;
      res3.a[i] = DEFAULT_VALUE;
      sign = -sign;
    }

  res1.x = INTRINSIC (_shuffle_epi8) (s1.x, s2.x);
  res2.x = INTRINSIC (_mask_shuffle_epi8) (res2.x, mask, s1.x, s2.x);
  res3.x = INTRINSIC (_maskz_shuffle_epi8) (mask, s1.x, s2.x);

  CALC (s1.a, s2.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_b) (res1, res_ref))
    abort ();

  MASK_MERGE (i_b) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_b) (res2, res_ref))
    abort ();

  MASK_ZERO (i_b) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_b) (res3, res_ref))
    abort ();
}
