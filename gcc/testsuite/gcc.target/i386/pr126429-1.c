/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bf16 -mavx512dq" } */
/* { dg-final { scan-assembler-not "kmovb" } } */

#include <immintrin.h>

__m512 dp16ps_mask (__m512 src, __mmask16 k, __m512bh a, __m512bh b)
{
  return _mm512_mask_dpbf16_ps (src, k, a, b);
}

__m512 dp16ps_maskz (__mmask16 k, __m512 src, __m512bh a, __m512bh b)
{
  return _mm512_maskz_dpbf16_ps (k, src, a, b);
}
