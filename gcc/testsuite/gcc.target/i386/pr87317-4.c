/* { dg-do compile } */
/* { dg-options "-O2 -march=haswell" } */
/* { dg-final { scan-assembler-times "vpmovzxbd" 1 } } */
/* { dg-final { scan-assembler-not "\tvmovd\t" } } */

#include <immintrin.h>

void
f (void *dst, void *ptr)
{
  __m128i data = _mm_cvtsi32_si128(*(int*)ptr);
  data = _mm_cvtepu8_epi32(data);
  _mm_storeu_si128((__m128i*)dst, data);
}
