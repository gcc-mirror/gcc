/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vfmsubadd...pd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\{\]" 8 } } */
/* { dg-final { scan-assembler-times "vfmsubadd...pd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 4 } } */
/* { dg-final { scan-assembler-times "vfmsubadd231pd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 2 } } */
/* { dg-final { scan-assembler-times "vfmsubadd...pd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 2 } } */
/* { dg-final { scan-assembler-times "vfmsubadd...pd\[ \\t\]+\[^\n\]*\{rn-sae\}\[^\n\]*%zmm\[0-9\]\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vfmsubadd...pd\[ \\t\]+\[^\n\]*\{rd-sae\}\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vfmsubadd231pd\[ \\t\]+\[^\n\]*\{ru-sae\}\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 1 } } */
/* { dg-final { scan-assembler-times "vfmsubadd...pd\[ \\t\]+\[^\n\]*\{rz-sae\}\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 1 } } */

#include <immintrin.h>

volatile __m512d x1, x2, x3;
volatile __mmask8 m;

void extern
avx512f_test (void)
{
  x1 = _mm512_fmsubadd_pd (x1, x2, x3);
  x1 = _mm512_mask_fmsubadd_pd (x1, m, x2, x3);
  x3 = _mm512_mask3_fmsubadd_pd (x1, x2, x3, m);
  x1 = _mm512_maskz_fmsubadd_pd (m, x1, x2, x3);
  x1 = _mm512_fmsubadd_round_pd (x1, x2, x3, _MM_FROUND_TO_NEAREST_INT | _MM_FROUND_NO_EXC);
  x1 = _mm512_mask_fmsubadd_round_pd (x1, m, x2, x3, _MM_FROUND_TO_NEG_INF | _MM_FROUND_NO_EXC);
  x3 = _mm512_mask3_fmsubadd_round_pd (x1, x2, x3, m, _MM_FROUND_TO_POS_INF | _MM_FROUND_NO_EXC);
  x1 = _mm512_maskz_fmsubadd_round_pd (m, x1, x2, x3, _MM_FROUND_TO_ZERO | _MM_FROUND_NO_EXC);
}
