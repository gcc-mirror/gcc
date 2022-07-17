/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times {(?n)vmovd[ \t]+} 3 } } */
/* { dg-final { scan-assembler-not {(?n)movq[ \t]+} } } */

#include<immintrin.h>

__m128i
foo1 (int* p)
{
  return _mm_set_epi64x (0, (unsigned int) ((*(__m32_u *)p)[0]));
}

__m256i
foo3 (int* p)
{
  return _mm256_set_epi64x (0, 0, 0, (unsigned int) ((*(__m32_u *)p)[0]));
}

__m512i
foo5 (int* p)
{
  return _mm512_set_epi64 (0, 0, 0, 0, 0, 0, 0,
			   (unsigned int) ((*(__m32_u *)p)[0]));
}
