/* { dg-do run } */
/* { dg-options "-mavxneconvert -O2" } */
/* { dg-require-effective-target avxneconvert } */
#define AVXNECONVERT
#include <stdint.h>

#ifndef CHECK
#define CHECK "avx-check.h"
#endif

#ifndef TEST
#define TEST avx_test
#endif

#include CHECK

typedef union
{
  uint32_t int32;
  float flt;
} float_int_t;

typedef union
{
  __m128bh  x;
  unsigned short a[8];
} union128bf16;

void TEST (void)
{
  union128 src_128;
  union256 src_256;
  union128bf16 dst_128, dst_256;
  uint16_t res_ref_128[8] = {0}, res_ref_256[8];
  float_int_t fp32;
  for (int i = 0; i < 4; i++)
  {
    fp32.flt = (float) 2 * i + 7 + i * 0.25;
    src_128.a[i] = fp32.flt;
    res_ref_128[i] = fp32.int32 >> 16;
    dst_128.a[i] = 117;
  }

  for (int i = 0; i < 8; i++)
  {
    fp32.flt = (float) 2 * i + 7 + i * 0.25;
    src_256.a[i] = fp32.flt;
    res_ref_256[i] = fp32.int32 >> 16;
    dst_256.a[i] = 117;
  }
  dst_128.x = _mm_cvtneps_avx_pbh (src_128.x);
  dst_256.x = _mm256_cvtneps_avx_pbh (src_256.x);

  if (checkVus (dst_128.a, res_ref_128, 8))
    abort();
  if (checkVus (dst_128.a, res_ref_128, 8))
    abort();
}
