/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O3 -march=haswell" } */
/* { dg-final { scan-assembler-times "vpmovsxwq" 1 } } */

#include <immintrin.h>

#define MAX 4

long long int dst[MAX];
short src[MAX];

void
foo (void)
{
  int i;
  for (i = 0; i < MAX; i += 4)
    {
      __m128i data = _mm_cvtsi64_si128(*(long long int*)(src + i));
      __m256i x = _mm256_cvtepi16_epi64(data);
      _mm256_storeu_si256((__m256i*)(dst + i), x);
    }
}
