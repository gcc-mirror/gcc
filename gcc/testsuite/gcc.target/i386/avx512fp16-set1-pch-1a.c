/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16" } */

#include <immintrin.h>

__m512h
__attribute__ ((noinline, noclone))
test_mm512_set1_pch (_Float16 _Complex A)
{
  return _mm512_set1_pch(A);
}

/* { dg-final { scan-assembler "vbroadcastss\[ \\t\]+\[^\n\r\]*%zmm\[01\]" } } */
