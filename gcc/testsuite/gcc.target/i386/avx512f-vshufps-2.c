/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

void
CALC (float *e, UNION_TYPE (AVX512F_LEN,) s1, UNION_TYPE (AVX512F_LEN,) s2,
      int imm)
{
  e[0] = s1.a[(imm >> 0) & 0x3];
  e[1] = s1.a[(imm >> 2) & 0x3];
  e[2] = s2.a[(imm >> 4) & 0x3];
  e[3] = s2.a[(imm >> 6) & 0x3];
#if AVX512F_LEN > 128
  e[4] = s1.a[4 + ((imm >> 0) & 0x3)];
  e[5] = s1.a[4 + ((imm >> 2) & 0x3)];
  e[6] = s2.a[4 + ((imm >> 4) & 0x3)];
  e[7] = s2.a[4 + ((imm >> 6) & 0x3)];
#if AVX512F_LEN > 256
  e[8] = s1.a[8 + ((imm >> 0) & 0x3)];
  e[9] = s1.a[8 + ((imm >> 2) & 0x3)];
  e[10] = s2.a[8 + ((imm >> 4) & 0x3)];
  e[11] = s2.a[8 + ((imm >> 6) & 0x3)];
  e[12] = s1.a[12 + ((imm >> 0) & 0x3)];
  e[13] = s1.a[12 + ((imm >> 2) & 0x3)];
  e[14] = s2.a[12 + ((imm >> 4) & 0x3)];
  e[15] = s2.a[12 + ((imm >> 6) & 0x3)];
#endif
#endif
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN,) u1, u2, u3, s1, s2;
  float e[SIZE];
  int i, sign;
  MASK_TYPE mask = MASK_VALUE;

  sign = -1;
  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = 1.5 + 34.67 * i * sign;
      s2.a[i] = -22.17 * i * sign;
      u1.a[i] = DEFAULT_VALUE;
      u2.a[i] = DEFAULT_VALUE;
      u3.a[i] = DEFAULT_VALUE;
      sign = sign * -1;
    }


  u1.x = INTRINSIC (_shuffle_ps) (s1.x, s2.x, 203);
  u2.x = INTRINSIC (_mask_shuffle_ps) (u2.x, mask, s1.x, s2.x, 203);
  u3.x = INTRINSIC (_maskz_shuffle_ps) (mask, s1.x, s2.x, 203);

  CALC (e, s1, s2, 203);

  if (UNION_CHECK (AVX512F_LEN,) (u1, e))
    abort ();

  MASK_MERGE ()(e, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN,) (u2, e))
    abort ();

  MASK_ZERO ()(e, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN,) (u3, e))
    abort ();
}
