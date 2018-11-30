/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-times "vpmovzxbq" 1 } } */
/* { dg-final { scan-assembler-not "vmovq" } } */

#include <immintrin.h>

void
f (void *dst, void *ptr)
{
  __m128i y = _mm_cvtsi64_si128(*(long long int*)ptr);
  __m512i z = _mm512_cvtepu8_epi64 (y);
  _mm512_storeu_si512((__m512i*)dst, z);
}
