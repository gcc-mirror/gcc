/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */

/* { dg-final { scan-assembler-not "vfmulcph\[ \\t\]+\[^\{\n\]*%zmm1+\[^\n\r]*%zmm0+\[^\n\r]*%zmm0+(?:\n|\[ \\t\]+#)" } } */
/* { dg-final { scan-assembler-not "vfmaddcph\[ \\t\]+\[^\{\n\]*%zmm0+\[^\n\r]*%zmm0+\[^\n\r]*%zmm0+(?:\n|\[ \\t\]+#)" } } */
/* { dg-final { scan-assembler-not "vfmulcsh\[ \\t\]+\[^\{\n\]*%xmm1+\[^\n\r]*%xmm0+\[^\n\r]*%xmm0+(?:\n|\[ \\t\]+#)" } } */
/* { dg-final { scan-assembler-not "vfmaddcsh\[ \\t\]+\[^\{\n\]*%xmm0+\[^\n\r]*%xmm0+\[^\n\r]*%xmm0+(?:\n|\[ \\t\]+#)" } } */

#include <immintrin.h>

volatile __m512h a1;
volatile __m128h a2;
__m512h b1;
__m128h b2;

void extern
avx512f_test (void)
{
  a1 = _mm512_fmul_pch (a1, a1);
  b1 = _mm512_fmadd_pch (b1, b1, b1);
  a2 = _mm_fmul_sch (a2, a2);
  b2 = _mm_fmadd_sch (b2, b2, b2);
}
