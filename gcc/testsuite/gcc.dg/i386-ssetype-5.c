/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -msse2 -march=athlon" } */
/* { dg-final { scan-assembler "pand.*\[bs\]p" } } */
/* { dg-final { scan-assembler "pandn.*\[bs\]p" } } */
/* { dg-final { scan-assembler "pxor.*\[bs\]p" } } */
/* { dg-final { scan-assembler "por.*\[bs\]p" } } */
/* { dg-final { scan-assembler "movdqa" } } */
/* { dg-final { scan-assembler-not "movaps.*\[bs\]p" } } */

/* Verify that we generate proper instruction with memory operand.  */

#include <xmmintrin.h>
__m128i
t1(__m128i a, __m128i b)
{
return _mm_and_si128 (a,b);
}
__m128i
t2(__m128i a, __m128i b)
{
return _mm_andnot_si128 (a,b);
}
__m128i
t3(__m128i a, __m128i b)
{
return _mm_or_si128 (a,b);
}
__m128i
t4(__m128i a, __m128i b)
{
return _mm_xor_si128 (a,b);
}

