/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vmovdqa" 4 } } */
/* { dg-final { scan-assembler-times {vpternlog[^\n\r]*\(%(?:r|e)dx\)} 2 { target { ! ia32 } } } } */

#include <immintrin.h>

__m512i f(__m512i* a, __m512i* b, __m512i* c)
{
	return _mm512_ternarylogic_epi64 (a[0], b[0], c[0], ~_MM_TERNLOG_B | ~_MM_TERNLOG_C);
}

__m512i g(__m512i* a, __m512i* b, __m512i* c)
{
	return _mm512_ternarylogic_epi64 (a[0], b[0], c[0], ~_MM_TERNLOG_A | ~_MM_TERNLOG_C);
}

__m512i h(__m512i* a, __m512i* b, __m512i* c)
{
	return _mm512_ternarylogic_epi64 (a[0], b[0], c[0], ~_MM_TERNLOG_A | ~_MM_TERNLOG_B);
}
