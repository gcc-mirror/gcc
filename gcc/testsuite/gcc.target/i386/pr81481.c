/* { dg-do compile } */
/* { dg-require-effective-target ssse3 } */
/* { dg-options "-O2 -fpic -mssse3" } */
/* { dg-final { scan-assembler-not "pshufb\[ \t\]\\(%esp\\)" } } */
#include <immintrin.h>

extern const signed char c[31] __attribute__((visibility("hidden")));

__m128i f(__m128i *x, void *v)
{
  int i;
  asm("# %0" : "=r"(i));
  __m128i t = _mm_loadu_si128((void*)&c[i]);
  __m128i xx = *x;
  xx =  _mm_shuffle_epi8(xx, t);
  asm("# %0 %1 %2" : "+x"(xx) : "r"(c), "r"(i));
  return xx;
}
