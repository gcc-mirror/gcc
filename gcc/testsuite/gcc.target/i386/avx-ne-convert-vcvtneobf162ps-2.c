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
  __m128bh x;
  uint32_t a[4];
} union128bf16_i;

typedef union
{
  __m256bh x;
  uint32_t a[8];
} union256bf16_i;

static uint16_t convert_fp32_to_bf16 (float fp)
{
  float_int_t fi;
  fi.flt = fp;
  return ((fi.int32 >> 16) & 0xffff);
}

void TEST (void)
{
  union128 dst_128;
  union256 dst_256;
  float res_ref_128[4], res_ref_256[8], fp32;
  uint16_t bf16;
  union128bf16_i src_128bh;
  union256bf16_i src_256bh;

  for (int i = 0; i < 4; i++)
  {
    fp32 = (float) 3 * i + 5 + i * 0.5;
    bf16 = convert_fp32_to_bf16 (fp32);
    // store bf16 at the upper part of the dword
    src_128bh.a[i] = (bf16 << 16) & 0xffff0000;
    res_ref_128[i] = fp32;
    dst_128.a[i] = 117;
  }
  for (int i = 0; i < 8; i++)
  {
    fp32 = (float) 3 * i + 5 + i * 0.5;
    bf16 = convert_fp32_to_bf16 (fp32);
    // store bf16 at the upper part of the dword
    src_256bh.a[i] = (bf16 << 16) & 0xffff0000;
    res_ref_256[i] = fp32;
    dst_256.a[i] = 117;
  }
  dst_128.x = _mm_cvtneobf16_ps (&src_128bh.x);
  dst_256.x = _mm256_cvtneobf16_ps (&src_256bh.x);
  if (check_union128 (dst_128, res_ref_128))
    abort();
  if (check_union256 (dst_256, res_ref_256))
    abort();
}
