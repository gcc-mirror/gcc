/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

void static
CALC (float *e, UNION_TYPE (AVX512F_LEN, d) s1)
{
  int i;
  for (i = 0; i < SIZE; i++)
    e[i] = (float) s1.a[i];
  for (i = SIZE; i < AVX512F_LEN_HALF / 32; i++)
    e[i] = 0.0;
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, d) s1;
  UNION_TYPE (AVX512F_LEN_HALF,) u1, u2, u3;
  MASK_TYPE mask = MASK_VALUE;
  float e[AVX512F_LEN_HALF / 32];
  int i;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = 0.12 * (i + 37.09);
      u1.a[i] = DEFAULT_VALUE;
      u2.a[i] = DEFAULT_VALUE;
      u3.a[i] = DEFAULT_VALUE;
    }

  u1.x = INTRINSIC (_cvtpd_ps) (s1.x);
  u2.x = INTRINSIC (_mask_cvtpd_ps) (u2.x, mask, s1.x);
  u3.x = INTRINSIC (_maskz_cvtpd_ps) (mask, s1.x);

  CALC (e, s1);

  if (UNION_CHECK (AVX512F_LEN_HALF,) (u1, e))
    abort ();

  MASK_MERGE ()(e, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN_HALF,) (u2, e))
    abort ();

  MASK_ZERO ()(e, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN_HALF,) (u3, e))
    abort ();
}
