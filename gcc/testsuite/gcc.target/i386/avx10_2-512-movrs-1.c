/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-march=x86-64-v3 -mavx10.2-512 -mmovrs -O2" } */
/* { dg-final { scan-assembler-times "vmovrsb\[ \\t\]\+\\(%(?:r|e).x\\), %zmm\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "vmovrsb\[ \\t\]\+\\(%(?:r|e).x\\), %zmm\[0-9\]+{%k\[1-7\]}" 2 } } */
/* { dg-final { scan-assembler-times "vmovrsb\[ \\t\]\+\\(%(?:r|e).x\\), %zmm\[0-9\]+{%k\[1-7\]}{z}" 1 } } */
/* { dg-final { scan-assembler-times "vmovrsd\[ \\t\]\+\\(%(?:r|e).x\\), %zmm\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "vmovrsd\[ \\t\]\+\\(%(?:r|e).x\\), %zmm\[0-9\]+{%k\[1-7\]}" 2 } } */
/* { dg-final { scan-assembler-times "vmovrsd\[ \\t\]\+\\(%(?:r|e).x\\), %zmm\[0-9\]+{%k\[1-7\]}{z}" 1 } } */
/* { dg-final { scan-assembler-times "vmovrsq\[ \\t\]\+\\(%(?:r|e).x\\), %zmm\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "vmovrsq\[ \\t\]\+\\(%(?:r|e).x\\), %zmm\[0-9\]+{%k\[1-7\]}" 2 } } */
/* { dg-final { scan-assembler-times "vmovrsq\[ \\t\]\+\\(%(?:r|e).x\\), %zmm\[0-9\]+{%k\[1-7\]}{z}" 1 } } */
/* { dg-final { scan-assembler-times "vmovrsw\[ \\t\]\+\\(%(?:r|e).x\\), %zmm\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "vmovrsw\[ \\t\]\+\\(%(?:r|e).x\\), %zmm\[0-9\]+{%k\[1-7\]}" 2 } } */
/* { dg-final { scan-assembler-times "vmovrsw\[ \\t\]\+\\(%(?:r|e).x\\), %zmm\[0-9\]+{%k\[1-7\]}{z}" 1 } } */

#include <immintrin.h>

__m512i *px;
volatile __m512i x;
volatile __mmask64 m1;
volatile __mmask16 m2;
volatile __mmask8 m3;
volatile __mmask32 m4;

void extern
avx512movrs_test (void)
{
  x = _mm512_loadrs_epi8(px);
  x = _mm512_mask_loadrs_epi8(x, m1, px);
  x = _mm512_maskz_loadrs_epi8(m1, px);
  x = _mm512_loadrs_epi32(px);
  x = _mm512_mask_loadrs_epi32(x, m2, px);
  x = _mm512_maskz_loadrs_epi32(m2, px);
  x = _mm512_loadrs_epi64(px);
  x = _mm512_mask_loadrs_epi64(x, m3, px);
  x = _mm512_maskz_loadrs_epi64(m3, px);
  x = _mm512_loadrs_epi16(px);
  x = _mm512_mask_loadrs_epi16(x, m4, px);
  x = _mm512_maskz_loadrs_epi16(m4, px);
}
