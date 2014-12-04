/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-march=*" } { "-march=x86-64" } } */
/* { dg-options "-O3 -march=x86-64 -msse2 -mno-ssse3" } */
/* { dg-final { scan-assembler "punpcklbw" } } */
extern void abort();
#include <emmintrin.h>
__m128i foo (char) __attribute__((noinline));
__m128i foo (char x) {
  return _mm_set1_epi8(x);
}
__m128i bar (char) __attribute__((noinline));
__m128i bar (char x) {
  return _mm_set_epi8 (x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x);
}

int
main() {
  int i, j;
  union u { __m128i v; char c[16]; };
  union u x, y;
  for (i = -128; i <= 127; i++)
    {
      x.v = foo ((char)i);
      y.v = bar ((char)i);
      for (j=0; j<16; j++)
	if (x.c[j] != y.c[j])
	  abort();
    }
  return 0;
}

