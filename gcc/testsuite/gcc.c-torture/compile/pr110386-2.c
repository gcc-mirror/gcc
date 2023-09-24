/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-mavx" } */

#include <immintrin.h>

__m128i do_stuff(__m128i XMM0) {
	__m128i ABS0 = _mm_abs_epi32(XMM0);
	__m128i MUL0 = _mm_mullo_epi32(ABS0, XMM0);
	__m128i MUL1 = _mm_mullo_epi32(MUL0, MUL0);
	return MUL1;
}
