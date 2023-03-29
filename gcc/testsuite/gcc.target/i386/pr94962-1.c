/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */
/* { dg-final { scan-assembler-not "vmovdqa" } } */
/* { dg-final { scan-assembler-times "vpcmpeqd\[ \\t\]+%xmm\[0-9\]" 1 } } */

#include <immintrin.h>

__m256i mask()
{
  return _mm256_zextsi128_si256(_mm_set1_epi8(-1));
}
