/* { dg-do compile } */
/* { dg-options "-mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vexpandpd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexpandpd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexpandpd\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexpandpd\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexpandpd\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexpandpd\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexpandpd\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexpandpd\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)"  1 } } */

#include <immintrin.h>

double *p;
volatile __m256d x1;
volatile __m128d x2;
volatile __mmask8 m;

void extern
avx512vl_test (void)
{
  x1 = _mm256_mask_expand_pd (x1, m, x1);
  x2 = _mm_mask_expand_pd (x2, m, x2);

  x1 = _mm256_maskz_expand_pd (m, x1);
  x2 = _mm_maskz_expand_pd (m, x2);

  x1 = _mm256_mask_expandloadu_pd (x1, m, p);
  x2 = _mm_mask_expandloadu_pd (x2, m, p);

  x1 = _mm256_maskz_expandloadu_pd (m, p);
  x2 = _mm_maskz_expandloadu_pd (m, p);
}
