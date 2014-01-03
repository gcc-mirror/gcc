/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

void static
CALC (float *e, float *s1, float *s2)
{
  int i;
  for (i = 0; i < SIZE / 4; i++)
    {
      e[4 * i] = s1[4 * i];
      e[4 * i + 1] = s2[4 * i];
      e[4 * i + 2] = s1[4 * i + 1];
      e[4 * i + 3] = s2[4 * i + 1];
    }
}

void static
TEST (void)
{
  UNION_TYPE (AVX512F_LEN,) s1, s2, u1, u2, u3;
  MASK_TYPE mask = MASK_VALUE;
  float e[SIZE];
  int i;
  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = i * 123.2 + 32.6;
      s2.a[i] = i + 2.5;
      u1.a[i]= DEFAULT_VALUE;
      u2.a[i]= DEFAULT_VALUE;
      u3.a[i]= DEFAULT_VALUE;
    }

  u1.x = INTRINSIC (_unpacklo_ps) (s1.x, s2.x);
  u2.x = INTRINSIC (_mask_unpacklo_ps) (u2.x, mask, s1.x, s2.x);
  u3.x = INTRINSIC (_maskz_unpacklo_ps) (mask, s1.x, s2.x);

  CALC (e, s1.a, s2.a);

  if (UNION_CHECK (AVX512F_LEN,) (u1, e))
    abort ();

  MASK_MERGE () (e, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, ) (u2, e))
    abort ();

  MASK_ZERO () (e, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, ) (u3, e))
    abort ();
}
