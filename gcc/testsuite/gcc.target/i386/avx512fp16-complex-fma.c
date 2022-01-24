/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2 -Ofast" } */
/* { dg-final { scan-assembler-times "vfmaddcph\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */
/* { dg-final { scan-assembler-not "vaddph\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"} } */
/* { dg-final { scan-assembler-not "vfmulcph\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)"} } */
/* { dg-final { scan-assembler-times "vfcmaddcph\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 2 } } */

#include <immintrin.h>
volatile __m512h x1, x2, res, a, b;
void extern
avx512f_test (void)
{
  res = _mm512_add_ph (x1, _mm512_fmadd_pch (a, b, _mm512_setzero_ph()));
  res = _mm512_add_ph (x1, _mm512_fcmadd_pch (a, b, _mm512_setzero_ph()));

  res = _mm512_add_ph (x1, _mm512_fmul_pch (a, b));
  res = _mm512_add_ph (x1, _mm512_fcmul_pch (a, b));
}
