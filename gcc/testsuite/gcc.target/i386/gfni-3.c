/* { dg-do compile } */
/* { dg-options "-mgfni -mavx -O2" } */
/* { dg-final { scan-assembler-times "vgf2p8affineinvqb\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgf2p8affineinvqb\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#include <x86intrin.h>

int *p;
volatile __m256i x3, x4;
volatile __m128i x5, x6;
 
void extern
avx512vl_test (void)
{
    x3 = _mm256_gf2p8affineinv_epi64_epi8(x3, x4, 3);
    x5 = _mm_gf2p8affineinv_epi64_epi8(x5, x6, 3);
}
