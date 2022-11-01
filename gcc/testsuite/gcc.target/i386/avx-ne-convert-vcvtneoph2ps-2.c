/* { dg-do run } */
/* { dg-options "-mavxneconvert -mf16c -O2" } */
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
  __m128h x;
  uint32_t a[4];
} union128h;

typedef union
{
  __m256h x;
  uint32_t a[8];
} union256h;

void TEST (void)
{
  union128 dst_128;
  union256 dst_256;
  float res_ref_128[4], res_ref_256[8], fp32;
  uint16_t fp16;
  union128h src_128h;
  union256h src_256h;

  for (int i = 0; i < 4; i++)
  {
    fp32 = (float) 3 * i + 5 + i * 0.5;
    fp16 = _cvtss_sh (fp32, 0);
    src_128h.a[i] = fp16 << 16;
    res_ref_128[i] = fp32;
    dst_128.a[i] = 117;
  }
  for (int i = 0; i < 8; i++)
  {
    fp32 = (float) 3 * i + 5 + i * 0.5;
    fp16 = _cvtss_sh (fp32, 0);
    src_256h.a[i] = fp16 << 16;
    res_ref_256[i] = fp32;
    dst_256.a[i] = 117;
  }
  dst_128.x = _mm_cvtneoph_ps (&src_128h.x);
  dst_256.x = _mm256_cvtneoph_ps (&src_256h.x);
  if (check_union128 (dst_128, res_ref_128))
    abort();
  if (check_union256 (dst_256, res_ref_256))
    abort();
}
