/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vmovdqu16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu16\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu16\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu16\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu16\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu16\[ \\t\]+\[^\{\n\]*\\)\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu16\[ \\t\]+\[^\{\n\]*\\)\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu16\[ \\t\]+\[^\{\n\]*\\)\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu16\[ \\t\]+\[^\{\n\]*\\)\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu16\[ \\t\]+\[^\{\n\]*\\)\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu16\[ \\t\]+\[^\{\n\]*\\)\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*\\)\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu16\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*\\)\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu16\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*\\)\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

short *p;
volatile __m512i x1, yy;
volatile __m256i x2, y2;
volatile __m128i x3, y3;
volatile __mmask32 m32;
volatile __mmask16 m16;
volatile __mmask8 m8;

void extern
avx512bw_test (void)
{
  x1 = _mm512_mask_mov_epi16 (x1, m32, yy);
  x2 = _mm256_mask_mov_epi16 (x2, m16, y2);
  x3 = _mm_mask_mov_epi16 (x3, m8, y3);

  x1 = _mm512_maskz_mov_epi16 (m32, yy);
  x2 = _mm256_maskz_mov_epi16 (m16, y2);
  x3 = _mm_maskz_mov_epi16 (m8, y3);

  x1 = _mm512_mask_loadu_epi16 (x1, m32, p);
  x2 = _mm256_mask_loadu_epi16 (x2, m16, p);
  x3 = _mm_mask_loadu_epi16 (x3, m8, p);

  x1 = _mm512_maskz_loadu_epi16 (m32, p);
  x2 = _mm256_maskz_loadu_epi16 (m16, p);
  x3 = _mm_maskz_loadu_epi16 (m8, p);

  _mm512_mask_storeu_epi16 (p, m32, x1);
  _mm256_mask_storeu_epi16 (p, m16, x2);
  _mm_mask_storeu_epi16 (p, m8, x3);
}
