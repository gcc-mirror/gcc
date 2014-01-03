/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vrndscalepd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\[^\n\]*%zmm\[0-9\]\[^\{\]" 6} } */
/* { dg-final { scan-assembler-times "vrndscalepd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 9} } */
/* { dg-final { scan-assembler-times "vrndscalepd\[ \\t\]+\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\{z\}" 3} } */
/* { dg-final { scan-assembler-times "vrndscalepd\[ \\t\]+\\S*,\[ \\t\]+\{sae\}\[^\n\]*%zmm\[0-9\]\[^\n\]*%zmm\[0-9\]\[^\{\]" 3} } */
/* { dg-final { scan-assembler-times "vrndscalepd\[ \\t\]+\\S*,\[ \\t\]+\{sae\}\[^\n\]*%zmm\[0-9\]\{%k\[1-7\]\}\[^\{\]" 6} } */

#include <immintrin.h>

volatile __m512d x;

void extern
avx512f_test (void)
{
  x = _mm512_roundscale_pd (x, 0x42);
  x = _mm512_ceil_pd (x);
  x = _mm512_floor_pd (x);
  x = _mm512_mask_roundscale_pd (x, 2, x, 0x42);
  x = _mm512_mask_ceil_pd (x, 2, x);
  x = _mm512_mask_floor_pd (x, 2, x);
  x = _mm512_maskz_roundscale_pd (2, x, 0x42);
  x = _mm512_maskz_ceil_pd (2, x);
  x = _mm512_maskz_floor_pd (2, x);

  x = _mm512_roundscale_round_pd (x, 0x42, _MM_FROUND_NO_EXC);
  x = _mm512_ceil_round_pd (x, _MM_FROUND_NO_EXC);
  x = _mm512_floor_round_pd (x, _MM_FROUND_NO_EXC);
  x = _mm512_mask_roundscale_round_pd (x, 2, x, 0x42, _MM_FROUND_NO_EXC);
  x = _mm512_mask_ceil_round_pd (x, 2, x, _MM_FROUND_NO_EXC);
  x = _mm512_mask_floor_round_pd (x, 2, x, _MM_FROUND_NO_EXC);
  x = _mm512_maskz_roundscale_round_pd (2, x, 0x42, _MM_FROUND_NO_EXC);
  x = _mm512_maskz_ceil_round_pd (2, x, _MM_FROUND_NO_EXC);
  x = _mm512_maskz_floor_round_pd (2, x, _MM_FROUND_NO_EXC);
}
