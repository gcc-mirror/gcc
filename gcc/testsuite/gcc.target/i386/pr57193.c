/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-sse3 -mtune=generic" } */
/* XFAIL the test due to PR87716.  */
/* { dg-final { scan-assembler-times "movdqa" 2 { xfail *-*-* } } } */

#include <emmintrin.h>

void test1(const __m128i* in1, const __m128i* in2, __m128i* out,
           __m128i f, __m128i zero)
{
	__m128i c = _mm_avg_epu8(*in1, *in2);
	__m128i l = _mm_unpacklo_epi8(c, zero);
	__m128i h = _mm_unpackhi_epi8(c, zero);
	__m128i m = _mm_mulhi_epu16(l, f);
	__m128i n = _mm_mulhi_epu16(h, f);
	*out = _mm_packus_epi16(m, n);
}
