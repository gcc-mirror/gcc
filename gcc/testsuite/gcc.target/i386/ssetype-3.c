/* { dg-do compile } */
/* This test checks for absolute memory operands.  */
/* { dg-require-effective-target nonpic } */
/* { dg-options "-O2 -msse2 -march=k8" } */
/* { dg-final { scan-assembler "andps\[^\\n\]*magic" } } */
/* { dg-final { scan-assembler "andnps\[^\\n\]*magic" } } */
/* { dg-final { scan-assembler "xorps\[^\\n\]*magic" } } */
/* { dg-final { scan-assembler "orps\[^\\n\]*magic" } } */
/* { dg-final { scan-assembler-not "movdqa" } } */
/* { dg-final { scan-assembler "movaps\[^\\n\]*magic" } } */

/* Verify that we generate proper instruction with memory operand.  */

#include <xmmintrin.h>

static __m128 magic_a, magic_b;
__m128
t1(void)
{
return _mm_and_ps (magic_a,magic_b);
}
__m128
t2(void)
{
return _mm_andnot_ps (magic_a,magic_b);
}
__m128
t3(void)
{
return _mm_or_ps (magic_a,magic_b);
}
__m128
t4(void)
{
return _mm_xor_ps (magic_a,magic_b);
}
