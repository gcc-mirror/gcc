/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2" } */
/* { dg-require-effective-target avx10_2 } */

#ifndef AVX10_2
#define AVX10_2
#define AVX10_2_512
#define AVX10_512BIT
#endif

#include "avx10-helper.h"

#define SRC_SIZE (AVX512F_LEN / 8)
#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

static void
CALC (int *r, int *dst, char *s1, unsigned char *s2)
{
  short tempres[SRC_SIZE];
  for (int i = 0; i < SRC_SIZE; i++)
    tempres[i] = (short) s1[i] * (unsigned short) s2[i];
  for (int i = 0; i < SIZE; i++)
    {
      long long test = (long long) dst[i] + tempres[i * 4] + tempres[i * 4 + 1]
				      + tempres[i * 4 + 2] + tempres[i * 4 + 3];
      r[i] = test;
    }
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_d) res1, res2, res3;
  UNION_TYPE (AVX512F_LEN, i_b) src1;
  UNION_TYPE (AVX512F_LEN, i_ub) src2;
  MASK_TYPE mask = MASK_VALUE;
  int res_ref[SIZE], res_ref2[SIZE];

  for (i = 0; i < SRC_SIZE; i++)
    {
      int sign = i % 2 ? 1 : -1;
      src1.a[i] = sign*10*i*i;
      src2.a[i] = 10 +  3*i*i + sign;
   }

  for (i = 0; i < SIZE; i++)
    {
      res1.a[i] = 0x7FFFFFFF;
      res2.a[i] = DEFAULT_VALUE;
      res3.a[i] = DEFAULT_VALUE;
    }

  CALC (res_ref, res1.a, src1.a, src2.a);
  CALC (res_ref2, res2.a, src1.a, src2.a);

  res1.x = INTRINSIC (_dpbsud_epi32) (res1.x, src1.x, src2.x);
  res2.x = INTRINSIC (_mask_dpbsud_epi32) (res2.x, mask, src1.x, src2.x);
  res3.x = INTRINSIC (_maskz_dpbsud_epi32) (mask, res3.x, src1.x, src2.x);

  if (UNION_CHECK (AVX512F_LEN, i_d) (res1, res_ref))
    abort ();

  MASK_MERGE (i_d) (res_ref2, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res2, res_ref2))
    abort ();

  MASK_ZERO (i_d) (res_ref2, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res3, res_ref2))
    abort ();
}
