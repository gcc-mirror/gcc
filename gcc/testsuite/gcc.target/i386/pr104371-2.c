/* { dg-do compile } */
/* { dg-options "-O2 -mavx2" } */
/* { dg-final { scan-assembler "vptest\[ \\t\]" } } */
/* { dg-final { scan-assembler-not "vpxor\[ \\t\]" } } */
/* { dg-final { scan-assembler-not "vpcmpeqb\[ \\t\]" } } */
/* { dg-final { scan-assembler-not "vpmovmskb\[ \\t\]" } } */

#include <immintrin.h>

int is_zero256(__m256i x)
{
  return _mm256_movemask_epi8(_mm256_cmpeq_epi8(x, _mm256_setzero_si256())) == 0xffffffff;
}
