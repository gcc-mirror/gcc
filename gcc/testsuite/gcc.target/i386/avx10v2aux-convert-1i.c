/* { dg-do compile } */
/* { dg-options "-mavx10v2aux -O2 -fno-fuse-ops-with-volatile-access" } */
/* { dg-final { scan-assembler-times "vunpackb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vunpackb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vunpackb\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vunpackb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vunpackb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vunpackb\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vunpackb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vunpackb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vunpackb\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128i x128i;
volatile __m256i x256i;
volatile __m512i x512i;
volatile __mmask16 m16;
volatile __mmask32 m32;
volatile __mmask64 m64;

void extern
avx10v2aux_vunpack_test (void)
{
  x128i = _mm_unpack_epi8 (x128i, 8);
  x128i = _mm_mask_unpack_epi8 (x128i, m16, x128i, 8);
  x128i = _mm_maskz_unpack_epi8 (m16, x128i, 8);

  x256i = _mm256_unpack_epi8 (x256i, 16);
  x256i = _mm256_mask_unpack_epi8 (x256i, m32, x256i, 16);
  x256i = _mm256_maskz_unpack_epi8 (m32, x256i, 16);

  x512i = _mm512_unpack_epi8 (x512i, 16);
  x512i = _mm512_mask_unpack_epi8 (x512i, m64, x512i, 16);
  x512i = _mm512_maskz_unpack_epi8 (m64, x512i, 16);
}
