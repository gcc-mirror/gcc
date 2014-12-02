/* { dg-do run } */
/* { dg-options "-O2 -mavx512vbmi" } */
/* { dg-require-effective-target avx512vbmi } */

#define AVX512VBMI

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 8)
#include "avx512f-mask-type.h"

void
CALC (char *ind, char *src, char *res)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      res[i] = src[ind[i] & (SIZE - 1)];
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_b) s1, s2, res1, res2, res3;
  char res_ref[SIZE];
  MASK_TYPE mask = MASK_VALUE;
  int i;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = i * i * i;
      s2.a[i] = i + 20;
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_permutexvar_epi8) (s1.x, s2.x);
  res2.x = INTRINSIC (_mask_permutexvar_epi8) (res2.x, mask, s1.x, s2.x);
  res3.x = INTRINSIC (_maskz_permutexvar_epi8) (mask, s1.x, s2.x);
  CALC (s1.a, s2.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_b) (res1, res_ref))
    abort ();

  MASK_MERGE (i_b)(res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_b) (res2, res_ref))
    abort ();

  MASK_ZERO (i_b)(res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_b) (res3, res_ref))
    abort ();
}
