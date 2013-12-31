/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vcompressps\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vcompressps\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */
/* { dg-final { scan-assembler-times "vcompressps\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\n\]*\\)\{%k\[1-7\]\}\[^\{\]" 1 } } */

#include <immintrin.h>

float *p;
volatile __m512 x;
volatile __mmask16 m;

void extern
avx512f_test (void)
{
  x = _mm512_mask_compress_ps (x, m, x);
  x = _mm512_maskz_compress_ps (m, x);

  _mm512_mask_compressstoreu_ps (p, m, x);
}
