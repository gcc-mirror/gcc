/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

#ifndef CTRL
#define CTRL 6
#endif

#undef mask_v
#define mask_v(pos) (((CTRL & (1ULL << (pos))) >> (pos)) << 1)

static void
CALC (double *s1, long long *s2, double *r)
{
  int i;

  for (i = 0; i < SIZE; i++)
    r[i] = s1[(2 * (i / 2)) + ((s2[i] & 0x02) >> 1)];
}

void
TEST (void)
{
  UNION_TYPE (AVX512F_LEN, d) s1, res1, res2, res3;
  UNION_TYPE (AVX512F_LEN, i_q) s2;
  MASK_TYPE mask = MASK_VALUE;
  double res_ref[SIZE];
  int i;

  for (i = 0; i < SIZE; i++)
    {
      s1.a[i] = i + 10.;
      s2.a[i] = mask_v (i);
      res2.a[i] = DEFAULT_VALUE;
    }

  res1.x = INTRINSIC (_permutevar_pd) (s1.x, s2.x);
  res2.x = INTRINSIC (_mask_permutevar_pd) (res2.x, mask, s1.x, s2.x);
  res3.x = INTRINSIC (_maskz_permutevar_pd) (mask, s1.x, s2.x);

  CALC (s1.a, s2.a, res_ref);

  if (UNION_CHECK (AVX512F_LEN, d) (res1, res_ref))
    abort ();

  MASK_MERGE (d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res2, res_ref))
    abort ();

  MASK_ZERO (d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, d) (res3, res_ref))
    abort ();
}
