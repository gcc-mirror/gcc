/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

#ifndef CTRL
#define CTRL 233
#endif

#undef mask_v
#define mask_v(pos) ((CTRL & (0x3 << (pos))) >> (pos))

static void
CALC (float *s1, int *s2, float *r)
{
  int i;

  for (i = 0; i < SIZE; i++)
    r[i] = s1[(4 * (i / 4)) + (s2[i] & 0x03)];
}

void static
TEST (void)
{
  UNION_TYPE (AVX512F_LEN,) s1, res1, res2, res3;
  UNION_TYPE (AVX512F_LEN, i_d) s2;
  MASK_TYPE mask = MASK_VALUE;
  float res_ref[SIZE];
  int i;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = i + 10.;
      s2.a[i] = mask_v (i);
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_permutevar_ps) (s1.x, s2.x);
  res2.x = INTRINSIC (_mask_permutevar_ps) (res2.x, mask, s1.x, s2.x);
  res3.x = INTRINSIC (_maskz_permutevar_ps) (mask, s1.x, s2.x);

  CALC (s1.a, s2.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN,) (res1, res_ref))
    abort ();

  MASK_MERGE ()(res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN,) (res2, res_ref))
    abort ();

  MASK_ZERO ()(res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN,) (res3, res_ref))
    abort ();
}
