/* { dg-do compile } */
/* { dg-options "-mavx512er -O2" } */
/* { dg-warning "AVX512ER support will be removed in GCC 15" "" { target *-*-* } 0 } */
/* { dg-final { scan-assembler-times "vrsqrt28ss\[ \\t\]+\[^\{^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrsqrt28ss\[ \\t\]+\[^\{\n\]*\{sae\}\[^\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrsqrt28ss\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrsqrt28ss\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrsqrt28ss\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vrsqrt28ss\[ \\t\]+\[^\n\]*\{sae\}\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

volatile __m128 x, y, z;
volatile __mmask8 m;

void extern
avx512er_test (void)
{
  x = _mm_rsqrt28_ss (x, y);
  x = _mm_rsqrt28_round_ss (x, y, _MM_FROUND_NO_EXC);
  x = _mm_mask_rsqrt28_ss (z, m, x, y);
  x = _mm_mask_rsqrt28_round_ss (z, m, x, y, _MM_FROUND_NO_EXC);
  x = _mm_maskz_rsqrt28_ss (m, x, y);
  x = _mm_maskz_rsqrt28_round_ss (m, x, y, _MM_FROUND_NO_EXC);
}
