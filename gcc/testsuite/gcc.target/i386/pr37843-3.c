/* Test for stack alignment with sibcall optimization.  */
/* { dg-do compile { target { ia32 && nonpic } } } */
/* { dg-options "-O2 -msse2 -mpreferred-stack-boundary=4 -mstackrealign" } */
/* { dg-final { scan-assembler-not "andl\[\\t \]*\\$-16,\[\\t \]*%\[re\]?sp" } } */
/* { dg-final { scan-assembler-not "call\[\\t \]*_?foo" } } */
/* { dg-final { scan-assembler "jmp\[\\t \]*_?foo" } } */

#include <emmintrin.h>

extern int foo (__m128, __m128, __m128, __m128);

int bar (__m128 x1, __m128 x2, __m128 x3, __m128 x4)
{
    return foo (x1, x2, x3, x4);
}
