/* { dg-do compile } */
/* { dg-options "-mavx512vbmi2 -O2" } */
/* { dg-final { scan-assembler-times "vpexpandb\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandb\[ \\t\]+\[^\{\n\(]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandw\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandw\[ \\t\]+\[^\{\n\(]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */

/* { dg-final { scan-assembler-times "vmov\[a-z0-9\]*\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  2 } } */
/* { dg-final { scan-assembler-times "vmov\[a-z0-9\]*\[ \\t\]+\[^\{\n\(]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  2 } } */
#include <immintrin.h>

char *pi8;
short *pi16;
volatile __m512i xi16, xi8;

void extern
avx512f_test (void)
{
  xi8 = _mm512_mask_expand_epi8 (xi8, 0, xi8);
  xi8 = _mm512_mask_expand_epi8 (xi8, -1, xi8);
  xi8 = _mm512_mask_expand_epi8 (xi8, (1 << 8) - 1, xi8);
  xi8 = _mm512_mask_expand_epi8 (xi8, (1 << 8) + 1, xi8);

  xi8 = _mm512_mask_expandloadu_epi8 (xi8, 0, pi8);
  xi8 = _mm512_mask_expandloadu_epi8 (xi8, -1, pi8);
  xi8 = _mm512_mask_expandloadu_epi8 (xi8, (1 << 6) - 1, pi8);
  xi8 = _mm512_mask_expandloadu_epi8 (xi8, (1 << 6) + 3, pi8);

  xi16 = _mm512_mask_expand_epi16 (xi16, 0, xi16);
  xi16 = _mm512_mask_expand_epi16 (xi16, -1, xi16);
  xi16 = _mm512_mask_expand_epi16 (xi16, (1 << 3) - 1, xi16);
  xi16 = _mm512_mask_expand_epi16 (xi16, (1 << 3) + 2, xi16);

  xi16 = _mm512_mask_expandloadu_epi16 (xi16, 0, pi16);
  xi16 = _mm512_mask_expandloadu_epi16 (xi16,  -1, pi16);
  xi16 = _mm512_mask_expandloadu_epi16 (xi16, (1 << 7) - 1, pi16);
  xi16 = _mm512_mask_expandloadu_epi16 (xi16, (1 << 7) + 7, pi16);
}
