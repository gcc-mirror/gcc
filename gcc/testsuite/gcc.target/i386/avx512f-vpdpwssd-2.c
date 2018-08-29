/* { dg-do run } */
/* { dg-options "-O2 -mavx512f -mavx512vnni" } */
/* { dg-require-effective-target avx512f } */
/* { dg-require-effective-target avx512vnni } */

#define AVX512F

#define AVX512VNNI
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 16)
#define SIZE_RES (AVX512F_LEN / 32)

#include "avx512f-mask-type.h"

static void
CALC (int *r, int *dst, short *s1, short *s2)
{
  short tempres[SIZE];
  for (int i = 0; i < SIZE; i++) {
    tempres[i] = ((int)(s1[i]) * (int)(s2[i]));
  }
  for (int i = 0; i < SIZE_RES; i++) {
    long long test = (long long)dst[i] + tempres[i*2] + tempres[i*2 + 1];
    r[i] = test;
  }
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_d) res1, res2, res3;
  UNION_TYPE (AVX512F_LEN, i_w) src1, src2;
  MASK_TYPE mask = MASK_VALUE;
  int res_ref[SIZE_RES];
  int res_ref2[SIZE_RES];

  for (i = 0; i < SIZE; i++)
    {
      src1.a[i] = 1 + i;
      src2.a[i] = 2 + 2*i;
    }

  for (i = 0; i < SIZE_RES; i++)
    {
      res1.a[i] = 0x7fffffff;
      res2.a[i] = DEFAULT_VALUE;
      res3.a[i] = DEFAULT_VALUE;
    }

  CALC (res_ref, res1.a, src1.a, src2.a);
  CALC (res_ref2, res2.a, src1.a, src2.a);

  res1.x = INTRINSIC (_dpwssd_epi32) (res1.x, src1.x, src2.x);
  res2.x = INTRINSIC (_mask_dpwssd_epi32) (res2.x, mask, src1.x, src2.x);
  res3.x = INTRINSIC (_maskz_dpwssd_epi32) (mask, res3.x, src1.x, src2.x);

  if (UNION_CHECK (AVX512F_LEN, i_d) (res1, res_ref))
    abort ();

  MASK_MERGE (i_d) (res_ref2, mask, SIZE_RES);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res2, res_ref2))
    abort ();

  MASK_ZERO (i_d) (res_ref2, mask, SIZE_RES);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res3, res_ref2))
    abort ();
}
