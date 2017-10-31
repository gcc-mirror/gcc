/* { dg-do compile } */
/* { dg-options "-mgfni -mavx512bw -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vgf2p8affineinvqb\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgf2p8affineinvqb\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgf2p8affineinvqb\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%ymm\[0-9\]+\[^\\n\\r]*%ymm\[0-9\]+\[^\\n\\r\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgf2p8affineinvqb\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgf2p8affineinvqb\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vgf2p8affineinvqb\[ \\t\]+\[^\{\n\]*\\\$3\[^\n\r]*%xmm\[0-9\]+\[^\\n\\r]*%xmm\[0-9\]+\[^\\n\\r\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <x86intrin.h>

int *p;
volatile __m256i x3, x4;
volatile __m128i x5, x6;
volatile __mmask32 m32;
volatile __mmask16 m16;
 
void extern
avx512vl_test (void)
{
    x3 = _mm256_gf2p8affineinv_epi64_epi8(x3, x4, 3);
    x3 = _mm256_mask_gf2p8affineinv_epi64_epi8(x3, m32, x4, x3, 3);
    x3 = _mm256_maskz_gf2p8affineinv_epi64_epi8(m32, x3, x4, 3);
    x5 = _mm_gf2p8affineinv_epi64_epi8(x5, x6, 3);
    x5 = _mm_mask_gf2p8affineinv_epi64_epi8(x5, m16, x6, x5, 3);
    x5 = _mm_maskz_gf2p8affineinv_epi64_epi8(m16, x5, x6, 3);
}
