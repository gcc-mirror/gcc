/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -msse2 -march=athlon" } */
/* { dg-final { scan-assembler "andps.*\[bs]p" } } */
/* { dg-final { scan-assembler "andnps.*\[bs]p" } } */
/* { dg-final { scan-assembler "xorps.*\[bs]p" } } */
/* { dg-final { scan-assembler "orps.\[b*s]p" } } */
/* { dg-final { scan-assembler-not "movdqa" } } */
/* { dg-final { scan-assembler "movaps.*\[bs]p" } } */

/* Verify that we generate proper instruction with memory operand.  */

#include <xmmintrin.h>
__m128
t1(__m128 a, __m128 b)
{
return _mm_and_ps (a,b);
}
__m128
t2(__m128 a, __m128 b)
{
return _mm_andnot_ps (a,b);
}
__m128
t3(__m128 a, __m128 b)
{
return _mm_or_ps (a,b);
}
__m128
t4(__m128 a, __m128 b)
{
return _mm_xor_ps (a,b);
}
