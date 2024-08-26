/* { dg-do run } */
/* { dg-options "-O2 -mavx10.2-512" } */
/* { dg-require-effective-target avx10_2_512 } */

#ifndef AVX10_2
#define AVX10_2
#define AVX10_2_512
#define AVX10_512BIT
#endif
#include "avx10-helper.h"
#include <stdint.h>

#define SIZE_RES (AVX512F_LEN / 16)

static void
CALC (_Float16 *res_ref, float *src1, float *src2)
{
  float fp32;
  int i;
  for (i = 0; i < SIZE_RES / 2; i++)
    {
      fp32 = (float) 2 * i + 7 + i * 0.5;
      res_ref[i] = fp32;
      src2[i] = fp32;
    }
  for (i = SIZE_RES / 2; i < SIZE_RES; i++)
    {
      fp32 = (float)2 * i + 7 + i * 0.5;
      res_ref[i] = fp32;
      src1[i - (SIZE_RES / 2)] = fp32;
    }
}

void
TEST (void)
{
  int i;
  UNION_TYPE (AVX512F_LEN, h) res1;
  UNION_TYPE (AVX512F_LEN, ) src1, src2;
  _Float16 res_ref[SIZE_RES];
  float fp32;
  
  for (i = 0; i < SIZE_RES; i++)
    res1.a[i] = 5;
  
  CALC (res_ref, src1.a, src2.a);
  
  res1.x = INTRINSIC (_cvtx2ps_ph) (src1.x, src2.x);
  if (UNION_CHECK (AVX512F_LEN, h) (res1, res_ref))
    abort ();
}
