/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vmovdqu8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[ \\t\]+\[^\{\n\]*\\)\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[ \\t\]+\[^\{\n\]*\\)\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[ \\t\]+\[^\{\n\]*\\)\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[ \\t\]+\[^\{\n\]*\\)\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[ \\t\]+\[^\{\n\]*\\)\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[ \\t\]+\[^\{\n\]*\\)\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*\\)\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*\\)\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*\\)\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[ \\t\]+\[^\{\n\]*\\)\[^\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu\[ \\t\]+\[^\{\n\]*\\)\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu8\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*\\)(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqu\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*\\)(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

char *p, *p1, *p2, *p3, *p4;
volatile __m512i x1, yy, zzz;
volatile __m256i x2, y2;
volatile __m128i x3, y3, xxx;
volatile __mmask64 m64;
volatile __mmask32 m32;
volatile __mmask16 m16;

void extern
avx512bw_test (void)
{
  x1 = _mm512_mask_mov_epi8 (x1, m64, yy);
  x2 = _mm256_mask_mov_epi8 (x2, m32, y2);
  x3 = _mm_mask_mov_epi8 (x3, m16, y3);

  x1 = _mm512_maskz_mov_epi8 (m64, yy);
  x2 = _mm256_maskz_mov_epi8 (m32, y2);
  x3 = _mm_maskz_mov_epi8 (m16, y3);

  x1 = _mm512_mask_loadu_epi8 (x1, m64, p);
  x2 = _mm256_mask_loadu_epi8 (x2, m32, p);
  x3 = _mm_mask_loadu_epi8 (x3, m16, p);

  x1 = _mm512_maskz_loadu_epi8 (m64, p);
  x2 = _mm256_maskz_loadu_epi8 (m32, p);
  x3 = _mm_maskz_loadu_epi8 (m16, p);

  zzz = _mm512_loadu_epi8 (p3);
  xxx = _mm_loadu_epi8 (p1);

  _mm512_mask_storeu_epi8 (p, m64, x1);
  _mm256_mask_storeu_epi8 (p, m32, x2);
  _mm_mask_storeu_epi8 (p, m16, x3);

  _mm512_storeu_epi8 (p4, zzz);
  _mm_storeu_epi8 (p2, xxx);
}
