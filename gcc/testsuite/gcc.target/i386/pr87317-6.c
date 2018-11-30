/* { dg-do compile } */
/* { dg-options "-O2 -march=haswell" } */
/* { dg-final { scan-assembler-times "vpmovzxbq" 1 } } */
/* { dg-final { scan-assembler-not "vmovq" } } */

#include <immintrin.h>

void
f (void *dst, void *ptr)
{
  __m128i y = _mm_cvtsi32_si128(*(int*)ptr);
  __m256i z = _mm256_cvtepu8_epi64 (y);
  _mm256_storeu_si256((__m256i*)dst, z);
}
