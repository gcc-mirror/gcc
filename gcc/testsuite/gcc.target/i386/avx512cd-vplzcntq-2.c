/* { dg-do run } */
/* { dg-options "-O2 -mavx512cd" } */
/* { dg-require-effective-target avx512cd } */

#define HAVE_512
#define AVX512CD

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"
#include <strings.h>

static void
CALC (long long *s, long long *r)
{
  int i, res;

  for (i = 0; i < SIZE; i++)
    {
      res = 0;
      while ((res < 64) && (((s[i] >> (63 - res)) & 1) == 0))
	++res;
      r[i] = res;
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, i_q) s, res1, res2, res3;
  long long res_ref[SIZE];
  MASK_TYPE mask = MASK_VALUE;
  int i;

  for (i = 0; i < SIZE; i++)
    {
      s.a[i] = 12345678 * (i % 5);
      res1.a[i] = DEFAULT_VALUE;
      res2.a[i] = DEFAULT_VALUE;
      res3.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_lzcnt_epi64) (s.x);
  res2.x = INTRINSIC (_mask_lzcnt_epi64) (res2.x, mask, s.x);
  res3.x = INTRINSIC (_maskz_lzcnt_epi64) (mask, s.x);

  CALC (s.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, i_q) (res1, res_ref))
    abort ();

  MASK_MERGE (i_q) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res2, res_ref))
    abort ();

  MASK_ZERO (i_q) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_q) (res3, res_ref))
    abort ();
}
