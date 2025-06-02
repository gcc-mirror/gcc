/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-march=x86-64-v3 -mavx10.2 -mmovrs -O2" } */
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

__m512i *px;
volatile __m512i x;
__m256i *px1;
volatile __m256i x1;
__m128i *px2;
volatile __m128i x2;
volatile __mmask8 m8;
volatile __mmask16 m16;
volatile __mmask32 m32;
volatile __mmask64 m64;


void extern
avx10_movrs_test (void)
{
  x = _mm512_loadrs_epi8(px);
  x = _mm512_mask_loadrs_epi8(x, m64, px);
  x = _mm512_maskz_loadrs_epi8(m64, px);
  x = _mm512_loadrs_epi32(px);
  x = _mm512_mask_loadrs_epi32(x, m16, px);
  x = _mm512_maskz_loadrs_epi32(m16, px);
  x = _mm512_loadrs_epi64(px);
  x = _mm512_mask_loadrs_epi64(x, m8, px);
  x = _mm512_maskz_loadrs_epi64(m8, px);
  x = _mm512_loadrs_epi16(px);
  x = _mm512_mask_loadrs_epi16(x, m32, px);
  x = _mm512_maskz_loadrs_epi16(m32, px);

  x1 = _mm256_loadrs_epi8(px1);
  x1 = _mm256_mask_loadrs_epi8(x1, m32, px1);
  x1 = _mm256_maskz_loadrs_epi8(m32, px1);
  x1 = _mm256_loadrs_epi32(px1);
  x1 = _mm256_mask_loadrs_epi32(x1, m8, px1);
  x1 = _mm256_maskz_loadrs_epi32(m8, px1);
  x1 = _mm256_loadrs_epi64(px1);
  x1 = _mm256_mask_loadrs_epi64(x1, m8, px1);
  x1 = _mm256_maskz_loadrs_epi64(m8, px1);
  x1 = _mm256_loadrs_epi16(px1);
  x1 = _mm256_mask_loadrs_epi16(x1, m16, px1);
  x1 = _mm256_maskz_loadrs_epi16(m16, px1);

  x2 = _mm_loadrs_epi8(px2);
  x2 = _mm_mask_loadrs_epi8(x2, m16, px2);
  x2 = _mm_maskz_loadrs_epi8(m16, px2);
  x2 = _mm_loadrs_epi32(px2);
  x2 = _mm_mask_loadrs_epi32(x2, m8, px2);
  x2 = _mm_maskz_loadrs_epi32(m8, px2);
  x2 = _mm_loadrs_epi64(px2);
  x2 = _mm_mask_loadrs_epi64(x2, m8, px2);
  x2 = _mm_maskz_loadrs_epi64(m8, px2);
  x2 = _mm_loadrs_epi16(px2);
  x2 = _mm_mask_loadrs_epi16(x2, m8, px2);
  x2 = _mm_maskz_loadrs_epi16(m8, px2);
}
