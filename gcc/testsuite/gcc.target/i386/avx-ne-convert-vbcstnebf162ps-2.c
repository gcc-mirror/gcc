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
  uint16_t var;
  fp32 = (float) 3 * 2 + 5.5;
  for (int i = 0; i < 4; i++)
  {
    res_ref_128[i] = fp32;
    dst_128.a[i] = 117;
  }
  for (int i = 0; i < 8; i++)
  {
    res_ref_256[i] = fp32;
    dst_256.a[i] = 117;
  }
  var = convert_fp32_to_bf16 (fp32);
  dst_128.x = _mm_bcstnebf16_ps (&var);
  dst_256.x = _mm256_bcstnebf16_ps (&var);
  if (check_union128 (dst_128, res_ref_128))
    abort();
  if (check_union256 (dst_256, res_ref_256))
    abort();
}
