/* { dg-do run } */
/* { dg-options "-O2 -mavx5124vnniw" } */
/* { dg-require-effective-target avx5124vnniw } */
/* { dg-warning "AVX5124VNNIW support will be removed in GCC 15" "" { target *-*-* } 0 } */

#define AVX5124VNNIW
#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 32)

#include "avx512f-mask-type.h"

void
CALC (short *src1, short* src2, short *src3,
      short *src4, int* prev_dst, short *mult, int *dst)
{
  int i;

  for (i = 0; i < SIZE; i++)
    {
      int p1dword, p2dword;
      dst[i] = prev_dst[i];
      p1dword = (int)(src1[2*i  ]) * (int)(mult[0]);
      p2dword = (int)(src1[2*i+1]) * (int)(mult[1]);
      dst[i] += p1dword + p2dword;

      p1dword = (int)(src2[2*i  ]) * (int)(mult[2]);
      p2dword = (int)(src2[2*i+1]) * (int)(mult[3]);
      dst[i] += p1dword + p2dword;

      p1dword = (int)(src3[2*i  ]) * (int)(mult[4]);
      p2dword = (int)(src3[2*i+1]) * (int)(mult[5]);
      dst[i] += p1dword + p2dword;

      p1dword = (int)(src4[2*i  ]) * (int)(mult[6]);
      p2dword = (int)(src4[2*i+1]) * (int)(mult[7]);
      dst[i] += p1dword + p2dword;
    }
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, i_w) src1, src2, src3, src4;
  UNION_TYPE (AVX512F_LEN, i_d) src5, dst, res1, res2, res3;
  UNION_TYPE (128, i_w) mult;
  MASK_TYPE mask = MASK_VALUE;
  int res_ref[SIZE];

  for (i = 0; i < SIZE * 2; i++)
    {
      src1.a[i] = 2 + 7 * i % 291;
      src2.a[i] = 3 + 11 * (i % 377) * i;
      src3.a[i] = src1.a[i] * src1.a[i];
      src4.a[i] = src2.a[i] * src2.a[i];
    }
  for (i = 0; i < 8; i++)
    mult.a[i] = 3 + i * 2;

  for (i = 0; i < SIZE; i++)
    src5.a[i] = DEFAULT_VALUE;

  CALC (src1.a, src2.a, src3.a, src4.a, src5.a, mult.a, res_ref);

  res1.x = INTRINSIC (_4dpwssd_epi32)       (      src5.x, src1.x, src2.x, src3.x, src4.x, &mult.x);
  res2.x = INTRINSIC (_mask_4dpwssd_epi32)  (src5.x, mask, src1.x, src2.x, src3.x, src4.x, &mult.x);
  res3.x = INTRINSIC (_maskz_4dpwssd_epi32) (mask, src5.x, src1.x, src2.x, src3.x, src4.x, &mult.x);

  if (UNION_CHECK (AVX512F_LEN, i_d) (res1, res_ref))
    abort ();

  MASK_MERGE (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res2, res_ref))
    abort ();

  MASK_ZERO (i_d) (res_ref, mask, SIZE);
  if (UNION_CHECK (AVX512F_LEN, i_d) (res3, res_ref))
    abort ();
}
