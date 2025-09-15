/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -mavx512dq" } */
/* { dg-final { scan-assembler-times "vpxor\[^\n\]*%ymm\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "vmovaps\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovaps\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vpxor\[^\n\]*%xmm\[0-9\]+" 3  } } */
/* { dg-final { scan-assembler-times "vmovaps\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vmovaps\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>
__m256h
__attribute__ ((noinline, noclone))
test_mm256_conj_pch (__m256h __A)
{
  return _mm256_conj_pch (__A);
}

__m128h
__attribute__ ((noinline, noclone))
test_mm_conj_pch (__m128h __A)
{
  return _mm_conj_pch (__A);
}

__m256h
__attribute__ ((noinline, noclone))
test_mm256_mask_conj_pch (__m256h __W, __mmask8 __U, __m256h __A)
{
  return _mm256_mask_conj_pch (__W, __U, __A);
}

__m128h
__attribute__ ((noinline, noclone))
test_mm_mask_conj_pch (__m128h __W, __mmask8 __U, __m128h __A)
{
  return _mm_mask_conj_pch (__W, __U, __A);
}

__m256h
__attribute__ ((noinline, noclone))
test_mm256_maskz_conj_pch (__mmask8 __U, __m256h __A)
{
   return _mm256_maskz_conj_pch (__U, __A);
}

__m128h
__attribute__ ((noinline, noclone))
test_mm_maskz_conj_pch (__mmask8 __U, __m128h __A) {
   return _mm_maskz_conj_pch (__U, __A);
}
