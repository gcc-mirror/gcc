/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times "vreduceph\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vreduceph\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vreduceph\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vreduceph\[ \\t\]+\[^\{\n\]*\{sae\}\[^\{\n\]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vreduceph\[ \\t\]+\[^\{\n\]*\{sae\}\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vreduceph\[ \\t\]+\[^\{\n\]*\{sae\}\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

#define IMM 123

volatile __m512h x1;
volatile __mmask32 m;

void extern
avx512fp16_test (void)
{
  x1 = _mm512_reduce_ph (x1, IMM);
  x1 = _mm512_mask_reduce_ph (x1, m, x1, IMM);
  x1 = _mm512_maskz_reduce_ph (m, x1, IMM);
  x1 = _mm512_reduce_round_ph (x1, IMM, 8);
  x1 = _mm512_mask_reduce_round_ph (x1, m, x1, IMM, 8);
  x1 = _mm512_maskz_reduce_round_ph (m, x1, IMM, 8);
}
