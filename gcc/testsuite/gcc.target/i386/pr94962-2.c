/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-not "vmovdqa" } } */
/* { dg-final { scan-assembler-times "vpcmpeqd\[ \\t\]+%xmm\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "vpcmpeqd\[ \\t\]+%ymm\[0-9\]" 1 } } */

#include <immintrin.h>

__m512i mask1()
{
  return _mm512_zextsi128_si512(_mm_set1_epi8(-1));
}

__m512i mask2()
{
  return _mm512_zextsi256_si512(_mm256_set1_epi8(-1));
}
