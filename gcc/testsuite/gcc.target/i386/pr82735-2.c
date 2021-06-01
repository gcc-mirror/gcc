/* { dg-do compile } */
/* { dg-options "-mavx -O2" } */

#include <immintrin.h>

void test(char *dest)
{
  /* xmm1 can be propagated to xmm2 by CSE.  */
  __m128i xmm1 = _mm_set_epi8(0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7, 0x8,
			      0x9, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16);
  _mm_storeu_si128((__m128i *)(dest + 32), xmm1);
  _mm256_zeroupper();
  _mm256_zeroupper();
  _mm256_zeroupper();
  _mm256_zeroupper();
  _mm256_zeroupper();
  __m128i xmm2 = xmm1;
  _mm_storeu_si128((__m128i *)dest, xmm2);
}

/* Darwin local constant symbol is "lC0", ELF targets ".LC0" */
/* { dg-final { scan-assembler-times {(?n)vmovdqa\t\.?[Ll]C0[^,]*, %xmm[0-9]} 1 } } */
