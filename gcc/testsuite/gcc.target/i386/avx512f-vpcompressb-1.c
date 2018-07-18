/* { dg-do compile } */
/* { dg-options "-mavx512f -mavx512vbmi2 -mavx512bw -O2" } */
/* { dg-final { scan-assembler-times "vpcompressb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcompressb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpcompressb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*\\)\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

int *p;
volatile __m512i x;
volatile __mmask64 m;

void extern
avx512f_test (void)
{
  x = _mm512_mask_compress_epi8 (x, m, x);
  x = _mm512_maskz_compress_epi8 (m, x);

  _mm512_mask_compressstoreu_epi8 (p, m, x);
}
