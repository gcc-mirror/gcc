/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpexpandq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandq\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandq\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandq\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandq\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandq\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandq\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */

#include <immintrin.h>

long long *p;
volatile __m256i x1;
volatile __m128i x2;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  x1 = _mm256_mask_expand_epi64 (x1, m, x1);
  x2 = _mm_mask_expand_epi64 (x2, m, x2);

  x1 = _mm256_maskz_expand_epi64 (m, x1);
  x2 = _mm_maskz_expand_epi64 (m, x2);

  x1 = _mm256_mask_expandloadu_epi64 (x1, m, p);
  x2 = _mm_mask_expandloadu_epi64 (x2, m, p);

  x1 = _mm256_maskz_expandloadu_epi64 (m, p);
  x2 = _mm_maskz_expandloadu_epi64 (m, p);
}
