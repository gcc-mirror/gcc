/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"
#define MASK ((1 << SIZE) - 1)
#include <x86intrin.h>

static void
CALC (double *s, double *r, MASK_TYPE mask)
{
  int i, k;

  for (i = 0, k = 0; i < SIZE; i++)
    {
      if (mask & (1 << i))
	r[k++] = s[i];
    }
}

static void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, d) s, res1, res2;
  double res3[SIZE];
  MASK_TYPE compressed_mask, mask = MASK_VALUE;
  double res_ref[SIZE];
  int i, mask_bit_count, sign = 1;

  for (i = 0; i < SIZE; i++)
    {
      s.a[i] = 12345 * (i + 200) * sign;
      res1.a[i] = DEFAULT_VALUE;
      res3[i] = DEFAULT_VALUE;
      sign = -sign;
    }

  res1.x = INTRINSIC (_mask_compress_pd) (res1.x, mask, s.x);
  res2.x = INTRINSIC (_maskz_compress_pd) (mask, s.x);
  INTRINSIC (_mask_compressstoreu_pd) (res3, mask, s.x);

  mask_bit_count = __popcntd (mask & MASK);
  compressed_mask = (1 << mask_bit_count) - 1;
  CALC (s.a, res_ref, mask);

  MASK_MERGE (d) (res_ref, compressed_mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res1, res_ref))
    abort ();

  MASK_ZERO (d) (res_ref, compressed_mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res2, res_ref))
    abort ();

  MASK_MERGE (d) (res_ref, compressed_mask, SIZE);
  if (checkVd (res3, res_ref, SIZE))
    abort ();
}
