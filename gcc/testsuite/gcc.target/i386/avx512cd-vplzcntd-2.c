/* { dg-do run } */
/* { dg-options "-O2 -mavx512cd" } */
/* { dg-require-effective-target avx512cd } */

#define HAVE_512
#define AVX512CD

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"
#include <strings.h>

static void
CALC (int *s, int *r)
{
  int i, res;

  for (i = 0; i < SIZE; i++)
    {
      res = 0;
      while ((res < 32) && (((s[i] >> (31 - res)) & 1) == 0))
	++res;
      r[i] = res;
    }
}

static void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_d) s, res1, res2, res3;
  int res_ref[SIZE];
  MASK_TYPE mask = MASK_VALUE;
  int i;

  for (i = 0; i < SIZE; i++)
    {
      s.a[i] = 12345678 * (i % 5);
      res1.a[i] = DEFAULT_VALUE;
      res2.a[i] = DEFAULT_VALUE;
      res3.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_lzcnt_epi32) (s.x);
  res2.x = INTRINSIC (_mask_lzcnt_epi32) (res2.x, mask, s.x);
  res3.x = INTRINSIC (_maskz_lzcnt_epi32) (mask, s.x);

  CALC (s.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_d) (res1, res_ref))
    abort ();

  MASK_MERGE (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res2, res_ref))
    abort ();

  MASK_ZERO (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res3, res_ref))
    abort ();
}
