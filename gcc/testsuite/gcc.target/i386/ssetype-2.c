/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -march=k8" } */
/* { dg-require-effective-target sse2 } */
/* { dg-final { scan-assembler "andpd" } } */
/* { dg-final { scan-assembler "andnpd" } } */
/* { dg-final { scan-assembler "xorpd" } } */
/* { dg-final { scan-assembler "orpd" } } */
/* { dg-final { scan-assembler-not "movdqa" } } */

/* Verify that we generate proper instruction without memory operand.  */

#include <xmmintrin.h>
__m128d
t1(__m128d a, __m128d b)
{
a=_mm_sqrt_pd(a);
b=_mm_sqrt_pd(b);
return _mm_and_pd (a,b);
}
__m128d
t2(__m128d a, __m128d b)
{
a=_mm_sqrt_pd(a);
b=_mm_sqrt_pd(b);
return _mm_andnot_pd (a,b);
}
__m128d
t3(__m128d a, __m128d b)
{
a=_mm_sqrt_pd(a);
b=_mm_sqrt_pd(b);
return _mm_or_pd (a,b);
}
__m128d
t4(__m128d a, __m128d b)
{
a=_mm_sqrt_pd(a);
b=_mm_sqrt_pd(b);
return _mm_xor_pd (a,b);
}
