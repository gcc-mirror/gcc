/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -msse2 -march=athlon" } */
/* { dg-final { scan-assembler "andpd.*\[bs\]p" } } */
/* { dg-final { scan-assembler "andnpd.*\[bs\]p" } } */
/* { dg-final { scan-assembler "xorpd.*\[bs\]p" } } */
/* { dg-final { scan-assembler "iorpd.*\[bs\]p" } } */
/* { dg-final { scan-assembler-not "movdqa" } } */
/* { dg-final { scan-assembler "movapd.*\[bs\]p" } } */

/* Verify that we generate proper instruction with memory operand.  */

#include <xmmintrin.h>
__m128d
t1(__m128d a, __m128d b)
{
return _mm_and_pd (a,b);
}
__m128d
t2(__m128d a, __m128d b)
{
return _mm_andnot_pd (a,b);
}
__m128d
t3(__m128d a, __m128d b)
{
return _mm_or_pd (a,b);
}
__m128d
t4(__m128d a, __m128d b)
{
return _mm_xor_pd (a,b);
}
