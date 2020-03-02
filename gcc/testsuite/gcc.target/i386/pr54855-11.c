/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16" } */
/* { dg-final { scan-assembler-times "vaddsh\[ \\t\]" 1 } } */
/* { dg-final { scan-assembler-not "vpextrw\[ \\t\]" } } */
/* { dg-final { scan-assembler-not "vmovd\[ \\t\]" } } */
/* { dg-final { scan-assembler-not "vpunpckldq\[ \\t\]" } } */
/* { dg-final { scan-assembler-not "vpunpcklqdq\[ \\t\]" } } */

#include <immintrin.h>

__m128h
foo (__m128h x, __m128h y)
{
  return _mm_add_sh (x, y);
}
