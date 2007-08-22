/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -march=k8" } */
/* { dg-final { scan-assembler "andps" } } */
/* { dg-final { scan-assembler "andnps" } } */
/* { dg-final { scan-assembler "xorps" } } */
/* { dg-final { scan-assembler "orps" } } */

/* Verify that we generate proper instruction without memory operand.  */

#include <xmmintrin.h>
__m128
t1(__m128 a, __m128 b)
{
a=_mm_sqrt_ps(a);
b=_mm_sqrt_ps(b);
return _mm_and_ps (a,b);
}
__m128
t2(__m128 a, __m128 b)
{
a=_mm_sqrt_ps(a);
b=_mm_sqrt_ps(b);
return _mm_andnot_ps (a,b);
}
__m128
t3(__m128 a, __m128 b)
{
a=_mm_sqrt_ps(a);
b=_mm_sqrt_ps(b);
return _mm_or_ps (a,b);
}
__m128
t4(__m128 a, __m128 b)
{
a=_mm_sqrt_ps(a);
b=_mm_sqrt_ps(b);
return _mm_xor_ps (a,b);
}
