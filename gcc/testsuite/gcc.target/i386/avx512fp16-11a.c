/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16" } */

#include <immintrin.h>
__m512h
__attribute__ ((noinline, noclone))
vadd512 (__m512h a, __m512h b)
{
  return a + b;
}

__m512h
__attribute__ ((noinline, noclone))
vsub512 (__m512h a, __m512h b)
{
  return a - b;
}

__m512h
__attribute__ ((noinline, noclone))
vmul512 (__m512h a, __m512h b)
{
  return a * b;
}

__m512h
__attribute__ ((noinline, noclone))
vdiv512 (__m512h a, __m512h b)
{
  return a / b;
}

/* { dg-final { scan-assembler-times "vaddph\[ \\t\]+\[^\n\r\]*%zmm\[01\]" 1 } } */
/* { dg-final { scan-assembler-times "vsubph\[ \\t\]+\[^\n\r\]*%zmm\[01\]" 1 } } */
/* { dg-final { scan-assembler-times "vmulph\[ \\t\]+\[^\n\r\]*%zmm\[01\]" 1 } } */
/* { dg-final { scan-assembler-times "vdivph\[ \\t\]+\[^\n\r\]*%zmm\[01\]" 1 } } */
