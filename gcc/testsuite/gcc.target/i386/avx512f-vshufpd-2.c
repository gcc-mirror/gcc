/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"
void static
CALC (double *e, UNION_TYPE (AVX512F_LEN, d) s1,
      UNION_TYPE (AVX512F_LEN, d) s2, int imm)
{
  e[0] = (imm & (1 << 0)) ? s1.a[1] : s1.a[0];
  e[1] = (imm & (1 << 1)) ? s2.a[1] : s2.a[0];
#if AVX512F_LEN > 128
  e[2] = (imm & (1 << 2)) ? s1.a[3] : s1.a[2];
  e[3] = (imm & (1 << 3)) ? s2.a[3] : s2.a[2];
#if AVX512F_LEN > 256
  e[4] = (imm & (1 << 4)) ? s1.a[5] : s1.a[4];
  e[5] = (imm & (1 << 5)) ? s2.a[5] : s2.a[4];
  e[6] = (imm & (1 << 6)) ? s1.a[7] : s1.a[6];
  e[7] = (imm & (1 << 7)) ? s2.a[7] : s2.a[6];
#endif
#endif
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, d) u1, u2, u3, s1, s2;
  double e[SIZE];
  MASK_TYPE mask = MASK_VALUE;
  int i;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = 2134.3343 * i + 54846.4641;
      s2.a[i] = 856.43576 * i + 1124.209;
      u1.a[i] = DEFAULT_VALUE;
      u2.a[i] = DEFAULT_VALUE;
      u3.a[i] = DEFAULT_VALUE;
    }

  u1.x = INTRINSIC (_shuffle_pd) (s1.x, s2.x, 120);
  u2.x = INTRINSIC (_mask_shuffle_pd) (u2.x, mask, s1.x, s2.x, 120);
  u3.x = INTRINSIC (_maskz_shuffle_pd) (mask, s1.x, s2.x, 120);
  CALC (e, s1, s2, 120);

  if (UNION_CHECK (AVX512F_LEN, d) (u1, e))
    abort ();

  MASK_MERGE (d) (e, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (u2, e))
    abort ();

  MASK_ZERO (d) (e, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (u3, e))
    abort ();
}
