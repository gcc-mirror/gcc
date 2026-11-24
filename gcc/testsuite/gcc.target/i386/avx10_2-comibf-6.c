/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -mavx10.2 -O2 -fno-trapping-math -ffinite-math-only" } */
/* { dg-final { scan-assembler-not "vucomxss" } } */
/* { dg-final { scan-assembler-times "vcomisbf16\[ \\t\]+\[^{}\n\]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 4 } } */

#include "avx10_2-comibf-5.c"
