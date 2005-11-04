/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O2 -msse2 -march=k8" } */
/* { dg-final { scan-assembler "andpd\[^\\n\]*magic" } } */
/* { dg-final { scan-assembler "andnpd\[^\\n\]*magic" } } */
/* { dg-final { scan-assembler "xorpd\[^\\n\]*magic" } } */
/* { dg-final { scan-assembler "orpd\[^\\n\]*magic" } } */
/* { dg-final { scan-assembler-not "movdqa" } } */
/* { dg-final { scan-assembler "movapd\[^\\n\]*magic" } } */

/* Verify that we generate proper instruction with memory operand.  */

#include <xmmintrin.h>

__m128d magic_a, magic_b;

__m128d
t1(void)
{
return _mm_and_pd (magic_a,magic_b);
}
__m128d
t2(void)
{
return _mm_andnot_pd (magic_a,magic_b);
}
__m128d
t3(void)
{
return _mm_or_pd (magic_a,magic_b);
}
__m128d
t4(void)
{
return _mm_xor_pd (magic_a,magic_b);
}
