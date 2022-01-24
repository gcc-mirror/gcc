/* { dg-do compile} */
/* { dg-options "-O2 -mavx512fp16" } */

#include <immintrin.h>
__m512h
__attribute__ ((noinline, noclone))
test_mm512_conj_pch (__m512h __A)
{
  return _mm512_conj_pch (__A);
}

/* { dg-final { scan-assembler-times "vpxord\[^\n\]*%zmm\[0-9\]+" 3 } } */

__m512h
__attribute__ ((noinline, noclone))
test_mm512_mask_conj_pch (__m512h __W, __mmask16 __U, __m512h __A)
{
  return _mm512_mask_conj_pch (__W, __U, __A);
}

/* { dg-final { scan-assembler-times "vpxord\[^\n\]*%zmm\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "kmovw\[^\n\]*%k\[1-9\]+" 2 } } */
/* { dg-final { scan-assembler-times "vmovaps\[^\n]" 2 } } */

__m512h
__attribute__ ((noinline, noclone))
test_mm512_maskz_conj_pch (__mmask16 __U, __m512h __A)
{
   return _mm512_maskz_conj_pch (__U, __A);
}

/* { dg-final { scan-assembler-times "vpxord\[^\n\]*%zmm\[0-9\]+" 3 } } */
/* { dg-final { scan-assembler-times "kmovw\[^\n\]*%k\[1-9\]+" 2 } } */
/* { dg-final { scan-assembler-times "vmovaps\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}\{z\}(?:\n|\[ \\t\]+#)" 1 } } */
