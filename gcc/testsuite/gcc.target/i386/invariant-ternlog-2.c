/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vmovdqa" 1 } } */
/* { dg-final { scan-assembler "vpternlog.*zmm0.*zmm0.*zmm0" } } */

#include <immintrin.h>

__m512i f(__m512i* a, __m512i* b, __m512i* c)
{
	return _mm512_ternarylogic_epi64 (a[0], b[0], c[0], ~_MM_TERNLOG_C);
}

