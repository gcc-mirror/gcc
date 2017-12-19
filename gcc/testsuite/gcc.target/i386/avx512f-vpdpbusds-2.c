/* { dg-do run } */
/* { dg-options "-O2 -mavx512f -mavx512vnni" } */
/* { dg-require-effective-target avx512f } */
/* { dg-require-effective-target avx512vnni } */

#define AVX512F

#define AVX512VNNI
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 8)
#define SIZE_RES (AVX512F_LEN / 32)

#include "avx512f-mask-type.h"

static void
CALC (int *r, int *dst, unsigned char *s1, char *s2)
{
  short tempres[SIZE];
  for (int i = 0; i < SIZE; i++) {
    tempres[i] = ((unsigned short)(s1[i]) * (short)(s2[i]));
  }
  for (int i = 0; i < SIZE_RES; i++) {
    long long test = (long long) dst[i] + tempres[i*4] + tempres[i*4 + 1] + tempres[i*4 + 2] + tempres[i*4 + 3];
    long long max_int = 0x7FFFFFFF;
    if (test > max_int)
      test = 0x7FFFFFFF;
    r[i] = test;
  }
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_d) res1, res2, res3;
  UNION_TYPE (AVX512F_LEN, i_ub) src1;
  UNION_TYPE (AVX512F_LEN, i_b) src2;
  MASK_TYPE mask = MASK_VALUE;
  int res_ref[SIZE_RES], res_ref2[SIZE_RES];

  for (i = 0; i < SIZE; i++)
    {
      int sign = i % 2 ? 1 : -1;
      src1.a[i] = 10 +  3*i*i + sign;
      src2.a[i] = sign*10*i*i;
   }

  for (i = 0; i < SIZE_RES; i++)
    {
      res1.a[i] = 0x7FFFFFFF;
      res2.a[i] = DEFAULT_VALUE;
      res3.a[i] = DEFAULT_VALUE;
    }

  CALC (res_ref, res1.a, src1.a, src2.a);
  CALC (res_ref2, res2.a, src1.a, src2.a);

  res1.x = INTRINSIC (_dpbusds_epi32) (res1.x, src1.x, src2.x);
  res2.x = INTRINSIC (_mask_dpbusds_epi32) (res2.x, mask, src1.x, src2.x);
  res3.x = INTRINSIC (_maskz_dpbusds_epi32) (mask, res3.x, src1.x, src2.x);

  if (UNION_CHECK (AVX512F_LEN, i_d) (res1, res_ref))
    abort ();

  MASK_MERGE (i_d) (res_ref2, mask, SIZE_RES);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res2, res_ref2))
    abort ();

  MASK_ZERO (i_d) (res_ref2, mask, SIZE_RES);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res3, res_ref2))
    abort ();
}
