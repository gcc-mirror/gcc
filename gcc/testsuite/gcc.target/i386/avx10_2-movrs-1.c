/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-march=x86-64-v3 -mavx10.2-256 -mmovrs -O2" } */
/* { dg-final { scan-assembler-times "vmovrsb\[ \\t\]\+\\(%(?:r|e).x\\), %ymm\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "vmovrsb\[ \\t\]\+\\(%(?:r|e).x\\), %ymm\[0-9\]+{%k\[1-7\]}" 2 } } */
/* { dg-final { scan-assembler-times "vmovrsb\[ \\t\]\+\\(%(?:r|e).x\\), %ymm\[0-9\]+{%k\[1-7\]}{z}" 1 } } */
/* { dg-final { scan-assembler-times "vmovrsd\[ \\t\]\+\\(%(?:r|e).x\\), %ymm\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "vmovrsd\[ \\t\]\+\\(%(?:r|e).x\\), %ymm\[0-9\]+{%k\[1-7\]}" 2 } } */
/* { dg-final { scan-assembler-times "vmovrsd\[ \\t\]\+\\(%(?:r|e).x\\), %ymm\[0-9\]+{%k\[1-7\]}{z}" 1 } } */
/* { dg-final { scan-assembler-times "vmovrsq\[ \\t\]\+\\(%(?:r|e).x\\), %ymm\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "vmovrsq\[ \\t\]\+\\(%(?:r|e).x\\), %ymm\[0-9\]+{%k\[1-7\]}" 2 } } */
/* { dg-final { scan-assembler-times "vmovrsq\[ \\t\]\+\\(%(?:r|e).x\\), %ymm\[0-9\]+{%k\[1-7\]}{z}" 1 } } */
/* { dg-final { scan-assembler-times "vmovrsw\[ \\t\]\+\\(%(?:r|e).x\\), %ymm\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "vmovrsw\[ \\t\]\+\\(%(?:r|e).x\\), %ymm\[0-9\]+{%k\[1-7\]}" 2 } } */
/* { dg-final { scan-assembler-times "vmovrsw\[ \\t\]\+\\(%(?:r|e).x\\), %ymm\[0-9\]+{%k\[1-7\]}{z}" 1 } } */
/* { dg-final { scan-assembler-times "vmovrsb\[ \\t\]\+\\(%(?:r|e).x\\), %xmm\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "vmovrsb\[ \\t\]\+\\(%(?:r|e).x\\), %xmm\[0-9\]+{%k\[1-7\]}" 2 } } */
/* { dg-final { scan-assembler-times "vmovrsb\[ \\t\]\+\\(%(?:r|e).x\\), %xmm\[0-9\]+{%k\[1-7\]}{z}" 1 } } */
/* { dg-final { scan-assembler-times "vmovrsd\[ \\t\]\+\\(%(?:r|e).x\\), %xmm\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "vmovrsd\[ \\t\]\+\\(%(?:r|e).x\\), %xmm\[0-9\]+{%k\[1-7\]}" 2 } } */
/* { dg-final { scan-assembler-times "vmovrsd\[ \\t\]\+\\(%(?:r|e).x\\), %xmm\[0-9\]+{%k\[1-7\]}{z}" 1 } } */
/* { dg-final { scan-assembler-times "vmovrsq\[ \\t\]\+\\(%(?:r|e).x\\), %xmm\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "vmovrsq\[ \\t\]\+\\(%(?:r|e).x\\), %xmm\[0-9\]+{%k\[1-7\]}" 2 } } */
/* { dg-final { scan-assembler-times "vmovrsq\[ \\t\]\+\\(%(?:r|e).x\\), %xmm\[0-9\]+{%k\[1-7\]}{z}" 1 } } */
/* { dg-final { scan-assembler-times "vmovrsw\[ \\t\]\+\\(%(?:r|e).x\\), %xmm\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "vmovrsw\[ \\t\]\+\\(%(?:r|e).x\\), %xmm\[0-9\]+{%k\[1-7\]}" 2 } } */
/* { dg-final { scan-assembler-times "vmovrsw\[ \\t\]\+\\(%(?:r|e).x\\), %xmm\[0-9\]+{%k\[1-7\]}{z}" 1 } } */

#include <immintrin.h>

__m256i *px1;
volatile __m256i x1;
__m128i *px2;
volatile __m128i x2;
volatile __mmask32 m1;
volatile __mmask8 m2;
volatile __mmask16 m3;


void extern
avx512movrs_test (void)
{
  x1 = _mm256_loadrs_epi8(px1);
  x1 = _mm256_mask_loadrs_epi8(x1, m1, px1);
  x1 = _mm256_maskz_loadrs_epi8(m1, px1);
  x1 = _mm256_loadrs_epi32(px1);
  x1 = _mm256_mask_loadrs_epi32(x1, m2, px1);
  x1 = _mm256_maskz_loadrs_epi32(m2, px1);
  x1 = _mm256_loadrs_epi64(px1);
  x1 = _mm256_mask_loadrs_epi64(x1, m2, px1);
  x1 = _mm256_maskz_loadrs_epi64(m2, px1);
  x1 = _mm256_loadrs_epi16(px1);
  x1 = _mm256_mask_loadrs_epi16(x1, m3, px1);
  x1 = _mm256_maskz_loadrs_epi16(m3, px1);

  x2 = _mm_loadrs_epi8(px2);
  x2 = _mm_mask_loadrs_epi8(x2, m3, px2);
  x2 = _mm_maskz_loadrs_epi8(m3, px2);
  x2 = _mm_loadrs_epi32(px2);
  x2 = _mm_mask_loadrs_epi32(x2, m2, px2);
  x2 = _mm_maskz_loadrs_epi32(m2, px2);
  x2 = _mm_loadrs_epi64(px2);
  x2 = _mm_mask_loadrs_epi64(x2, m2, px2);
  x2 = _mm_maskz_loadrs_epi64(m2, px2);
  x2 = _mm_loadrs_epi16(px2);
  x2 = _mm_mask_loadrs_epi16(x2, m2, px2);
  x2 = _mm_maskz_loadrs_epi16(m2, px2);
}
