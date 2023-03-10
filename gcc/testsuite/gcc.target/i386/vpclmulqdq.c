/* { dg-do compile } */
/* { dg-options "-mvpclmulqdq -mavx512vl -mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpclmulqdq\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpclmulqdq\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpclmulqdq\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#include <x86intrin.h>

volatile __m512i x1, x2;
volatile __m256i x3, x4;
volatile __m128i x5, x6;

void extern
avx512vl_test (void)
{
    x1 = _mm512_clmulepi64_epi128(x1, x2, 3);
    x3 = _mm256_clmulepi64_epi128(x3, x4, 3);
    x5 = _mm_clmulepi64_si128(x5, x6, 3);
}

