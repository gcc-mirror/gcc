/* PR target/84786 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mavx512f -mno-avx512vl -O2" } */

#include <x86intrin.h>

__m512i v;
__m128i w;

__m128i
foo (__m128i x, int y)
{
  __m128i z;
#define A(n) register __m512i zmm##n __asm ("zmm" #n);
#define B A(1) A(2) A(3) A(4) A(5) A(6) A(7) \
	  A(8) A(9) A(10) A(11) A(12) A(13) A(14)
  B
#undef A
#define A(n) asm volatile ("" : "=v" (zmm##n) : "0" (v));
  B
  asm volatile ("" : "=x" (z) : "0" (w));
  x = _mm_srli_epi16 (x, y);
  asm volatile ("" : : "x" (z));
#undef A
#define A(n) asm volatile ("" : : "v" (zmm##n));
  B
  return x;
}

__m256i
bar (__m256i x, int y)
{
  __m128i z;
#undef A
#define A(n) register __m512i zmm##n __asm ("zmm" #n);
  B
#undef A
#define A(n) asm volatile ("" : "=v" (zmm##n) : "0" (v));
  B
  asm volatile ("" : "=x" (z) : "0" (w));
  x = _mm256_slli_epi16 (x, y);
  asm volatile ("" : : "x" (z));
#undef A
#define A(n) asm volatile ("" : : "v" (zmm##n));
  B
  return x;
}

/* { dg-final { scan-assembler-not "vpsrlw\[\^\n\r]*xmm(1\[6-9]|\[23]\[0-9])" } } */
/* { dg-final { scan-assembler-not "vpsllw\[\^\n\r]*xmm(1\[6-9]|\[23]\[0-9])" } } */
