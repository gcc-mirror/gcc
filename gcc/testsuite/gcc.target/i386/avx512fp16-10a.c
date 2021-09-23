/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16" } */

#include <immintrin.h>

__m128h
__attribute__ ((noinline, noclone))
set_128 (_Float16 x)
{
  return _mm_set_sh (x);
}

/* { dg-final { scan-assembler-times "vmovw\[ \t]\+\[^\n\r]*xmm0" 1 { target { ia32 } } } } */
/* { dg-final { scan-assembler-times "vmovw\[ \t]\+\[^\n\r]*xmm0" 2 { target { ! ia32 } } } } */
