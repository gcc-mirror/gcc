/* { dg-do compile } */
/* { dg-options "-mavx512vbmi2 -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpexpandb\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandb\[ \\t\]+\[^\{\n\(]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandw\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandw\[ \\t\]+\[^\{\n\(]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */

/* { dg-final { scan-assembler-times "vmov\[a-z0-9\]*\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  2 } } */
/* { dg-final { scan-assembler-times "vmov\[a-z0-9\]*\[ \\t\]+\[^\{\n\(]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  2 } } */
/* { dg-final { scan-assembler-times "vpexpandb\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandb\[ \\t\]+\[^\{\n\(]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandw\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandw\[ \\t\]+\[^\{\n\(]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */

/* { dg-final { scan-assembler-times "vmov\[a-z0-9\]*\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  2 } } */
/* { dg-final { scan-assembler-times "(?:vmov\[a-z0-9\]*\[ \\t\]+\[^\{\n\(]*%xmm\[0-9\]+\{%k\[1-7\]\}|blend\[a-z]*\[ \\t\]+\[^\{\n\(]*%xmm\[0-9\]+)(?:\n|\[ \\t\]+#)"  2 } } */
#include <immintrin.h>

char *pi8;
short *pi16;
volatile __m256i xi16, xi8;
volatile __m128i xi16_xmm, xi8_xmm;

void extern
avx512f_test (void)
{
  xi8 = _mm256_mask_expand_epi8 (xi8, 0, xi8);
  xi8 = _mm256_mask_expand_epi8 (xi8, -1, xi8);
  xi8 = _mm256_mask_expand_epi8 (xi8, (1 << 30) - 1, xi8);
  xi8 = _mm256_mask_expand_epi8 (xi8, (1 << 16) + 1, xi8);

  xi8 = _mm256_mask_expandloadu_epi8 (xi8, 0, pi8);
  xi8 = _mm256_mask_expandloadu_epi8 (xi8, -1, pi8);
  xi8 = _mm256_mask_expandloadu_epi8 (xi8, (1 << 28) - 1, pi8);
  xi8 = _mm256_mask_expandloadu_epi8 (xi8, (1 << 15) + 3, pi8);

  xi16 = _mm256_mask_expand_epi16 (xi16, 0, xi16);
  xi16 = _mm256_mask_expand_epi16 (xi16, -1, xi16);
  xi16 = _mm256_mask_expand_epi16 (xi16, (1 << 15) - 1, xi16);
  xi16 = _mm256_mask_expand_epi16 (xi16, (1 << 14) + 2, xi16);

  xi16 = _mm256_mask_expandloadu_epi16 (xi16, 0, pi16);
  xi16 = _mm256_mask_expandloadu_epi16 (xi16, (1 << 16) - 1, pi16);
  xi16 = _mm256_mask_expandloadu_epi16 (xi16, (1 << 14) - 1, pi16);
  xi16 = _mm256_mask_expandloadu_epi16 (xi16, (1 << 13) + 7, pi16);

  xi8_xmm = _mm_mask_expand_epi8 (xi8_xmm, 0, xi8_xmm);
  xi8_xmm = _mm_mask_expand_epi8 (xi8_xmm, -1, xi8_xmm);
  xi8_xmm = _mm_mask_expand_epi8 (xi8_xmm, (1 << 13) - 1, xi8_xmm);
  xi8_xmm = _mm_mask_expand_epi8 (xi8_xmm, (1 << 12) + 1, xi8_xmm);

  xi8_xmm = _mm_mask_expandloadu_epi8 (xi8_xmm, 0, pi8);
  xi8_xmm = _mm_mask_expandloadu_epi8 (xi8_xmm, (1 << 16) - 1, pi8);
  xi8_xmm = _mm_mask_expandloadu_epi8 (xi8_xmm, (1 << 12) - 1, pi8);
  xi8_xmm = _mm_mask_expandloadu_epi8 (xi8_xmm, (1 << 11) + 3, pi8);

  xi16_xmm = _mm_mask_expand_epi16 (xi16_xmm, 0, xi16_xmm);
  xi16_xmm = _mm_mask_expand_epi16 (xi16_xmm, -1, xi16_xmm);
  xi16_xmm = _mm_mask_expand_epi16 (xi16_xmm, (1 << 7) - 1, xi16_xmm);
  xi16_xmm = _mm_mask_expand_epi16 (xi16_xmm, (1 << 4) + 2, xi16_xmm);

  xi16_xmm = _mm_mask_expandloadu_epi16 (xi16_xmm, 0, pi16);
  xi16_xmm = _mm_mask_expandloadu_epi16 (xi16_xmm, (1 << 8) - 1, pi16);
  xi16_xmm = _mm_mask_expandloadu_epi16 (xi16_xmm, (1 << 3) - 1, pi16);
  xi16_xmm = _mm_mask_expandloadu_epi16 (xi16_xmm, (1 << 6) + 7, pi16);
}
