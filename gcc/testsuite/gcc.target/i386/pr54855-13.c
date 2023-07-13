/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16" } */
/* { dg-final { scan-assembler-times "vm\[ai\]\[nx\]sh\[ \\t\]" 1 } } */
/* { dg-final { scan-assembler-not "vcomish\[ \\t\]" } } */
/* { dg-final { scan-assembler-not "vmovsh\[ \\t\]" { target { ! ia32 } } } } */

#include <immintrin.h>

__m128h
foo (__m128h x, __m128h y)
{
  x[0] = x[0] > y[0] ? x[0] : y[0];
  return x;
}
