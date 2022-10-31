/* { dg-do run } */
/* { dg-options "-mavxneconvert -mf16c -O2" } */
/* { dg-require-effective-target avxneconvert } */
#define AVXNECONVERT
#include <stdint.h>
#include <immintrin.h>

#ifndef CHECK
#define CHECK "avx-check.h"
#endif

#ifndef TEST
#define TEST avx_test
#endif

#include CHECK

void TEST (void)
{
  union128 dst_128;
  union256 dst_256;
  float res_ref_128[4], res_ref_256[8], fp32;
  uint16_t var;
  fp32 = (float) 3 * 2 + 8.5;
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
  var = _cvtss_sh (fp32, 0);
  dst_128.x = _mm_bcstnesh_ps (&var);
  dst_256.x = _mm256_bcstnesh_ps (&var);
  if (check_union128 (dst_128, res_ref_128))
    abort();
  if (check_union256 (dst_256, res_ref_256))
    abort();
}
