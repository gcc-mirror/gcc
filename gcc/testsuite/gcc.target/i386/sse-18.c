/* { dg-do run } */
/* { dg-options "-O3 -msse2" } */
extern void abort(void);
#include <emmintrin.h>
#include "../../gcc.dg/i386-cpuid.h"
__m128i foo (char) __attribute__((noinline));
__m128i foo (char x) {
  return _mm_set1_epi8(x);
}
__m128i bar (char) __attribute__((noinline));
__m128i bar (char x) {
  return _mm_set_epi8 (x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x);
}

main() {
  int i, j;
  union u { __m128i v; char c[16]; };
  union u x, y;
  unsigned long cpu_facilities;

  cpu_facilities = i386_cpuid ();

  if ((cpu_facilities & (bit_MMX | bit_SSE | bit_CMOV))
      != (bit_MMX | bit_SSE | bit_CMOV))
    /* If host has no vector support, pass.  */
    return 0;

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

