/* { dg-do run } */
/* { dg-options "-O2 -mavx512dq" } */
/* { dg-require-effective-target avx512dq } */

#define AVX512DQ
#include "avx512f-helper.h"

#define SIZE    (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

void
CALC (double *src1, double *src2, double *dst)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      union U { double d; long long l; } u1, u2;
      u1.d = src1[i];
      u2.d = src2[i];
      u1.l |= u2.l;
      dst[i] = u1.d;
    }
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN,d) s1, s2, res1, res2, res3;
  MASK_TYPE mask = MASK_VALUE;
  double dst_ref[SIZE];
  int i;

  for (i = 0; i < SIZE; i++) {
      s1.a[i] = 132.45 * i;
      s2.a[i] = 43.6 - i * 4.4;
      res2.a[i] = DEFAULT_VALUE;
  }

  res1.x = INTRINSIC (_or_pd) (s1.x, s2.x);
  res2.x = INTRINSIC (_mask_or_pd) (res2.x, mask, s1.x, s2.x);
  res3.x = INTRINSIC (_maskz_or_pd) (mask, s1.x, s2.x);

  CALC (s1.a, s2.a, dst_ref);

  if (UNION_CHECK (AVX512F_LEN,d) (res1, dst_ref))
    abort ();

  MASK_MERGE (d) (dst_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN,d) (res2, dst_ref))
    abort ();

  MASK_ZERO (d) (dst_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN,d) (res3, dst_ref))
    abort ();
}
