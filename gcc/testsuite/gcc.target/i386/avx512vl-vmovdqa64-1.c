/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vmovdqa64\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa64\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa64\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa64\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa\[ \\t\]+\\(\[^\n\]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 { target nonpic } } } */
/* { dg-final { scan-assembler-times "vmovdqa\[ \\t\]+\\(\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 { target nonpic } } } */
/* { dg-final { scan-assembler-times "vmovdqa64\[ \\t\]+\[^\{\n\]*\\)\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa64\[ \\t\]+\[^\{\n\]*\\)\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa64\[ \\t\]+\[^\{\n\]*\\)\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa64\[ \\t\]+\[^\{\n\]*\\)\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\nxy\]*\\(.{5,6}(?:\n|\[ \\t\]+#)" 1 { target nonpic } } } */
/* { dg-final { scan-assembler-times "vmovdqa64\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*\\)\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovdqa64\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*\\)\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

long long *p;
__m256i const *p1;
__m128i const *p2;
volatile __m256i yy, y2;
volatile __m128i xx, x2;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  yy = _mm256_mask_mov_epi64 (yy, m, y2);
  xx = _mm_mask_mov_epi64 (xx, m, x2);

  yy = _mm256_maskz_mov_epi64 (m, y2);
  xx = _mm_maskz_mov_epi64 (m, x2);

  yy = _mm256_load_si256 (p1);
  xx = _mm_load_si128 (p2);

  yy = _mm256_load_epi64 (p);
  xx = _mm_load_epi64 (p);

  yy = _mm256_mask_load_epi64 (yy, m, p);
  xx = _mm_mask_load_epi64 (xx, m, p);

  yy = _mm256_maskz_load_epi64 (m, p);
  xx = _mm_maskz_load_epi64 (m, p);

  _mm256_store_epi64 (p, yy);
  _mm_store_epi64 (p, xx);

  _mm256_mask_store_epi64 (p, m, yy);
  _mm_mask_store_epi64 (p, m, xx);
}
