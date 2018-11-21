/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=haswell" } */
/* { dg-final { scan-assembler-times "vpmovsxdq" 1 } } */
/* { dg-final { scan-assembler-not "vmovq" } } */

#include <immintrin.h>

void
f (void *dst, void *ptr)
{
  __m128i data = _mm_cvtsi64_si128(*(long long int*)ptr);
  data = _mm_cvtepi32_epi64(data);
  _mm_storeu_si128((__m128i*)dst, data);
}
