/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mavx512fp16 -O2" } */
/* { dg-final { scan-assembler-times {(?n)vmovw[ \t]+} 6 } } */
/* { dg-final { scan-assembler-not {(?n)mov[dq][ \t]+} } } */

#include<immintrin.h>
__m128i
foo (short* p)
{
  return _mm_set_epi32 (0, 0, 0, (unsigned short) ((*(__m16_u *)p)[0]));
}

__m128i
foo1 (short* p)
{
  return _mm_set_epi64x (0, (unsigned short) ((*(__m16_u *)p)[0]));
}

__m256i
foo2 (short* p)
{
  return _mm256_set_epi32 (0, 0, 0, 0, 0, 0, 0,
			   (unsigned short) ((*(__m16_u *)p)[0]));
}

__m256i
foo3 (short* p)
{
  return _mm256_set_epi64x (0, 0, 0, (unsigned short) ((*(__m16_u *)p)[0]));
}

__m512i
foo4 (short* p)
{
  return _mm512_set_epi32 (0, 0, 0, 0, 0, 0, 0, 0,
			   0, 0, 0, 0, 0, 0, 0,
			   (unsigned short) ((*(__m16_u *)p)[0]));
}

__m512i
foo5 (short* p)
{
  return _mm512_set_epi64 (0, 0, 0, 0, 0, 0, 0,
			   (unsigned short) ((*(__m16_u *)p)[0]));
}
