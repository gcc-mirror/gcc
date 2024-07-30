/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl" } */

#include <immintrin.h>
__m128h
__attribute__ ((noinline, noclone))
vadd128 (__m128h a, __m128h b)
{
  return a + b;
}

__m256h
__attribute__ ((noinline, noclone))
vadd256 (__m256h a, __m256h b)
{
  return a + b;
}

__m128h
__attribute__ ((noinline, noclone))
vsub128 (__m128h a, __m128h b)
{
  return a - b;
}

__m256h
__attribute__ ((noinline, noclone))
vsub256 (__m256h a, __m256h b)
{
  return a - b;
}

__m128h
__attribute__ ((noinline, noclone))
vmul128 (__m128h a, __m128h b)
{
  return a * b;
}

__m256h
__attribute__ ((noinline, noclone))
vmul256 (__m256h a, __m256h b)
{
  return a * b;
}

__m128h
__attribute__ ((noinline, noclone))
vdiv128 (__m128h a, __m128h b)
{
  return a / b;
}

__m256h
__attribute__ ((noinline, noclone))
vdiv256 (__m256h a, __m256h b)
{
  return a / b;
}

/* { dg-final { scan-assembler-times "vaddph\[ \\t\]+\[^\n\r\]*%xmm\[01\]" 1 } } */
/* { dg-final { scan-assembler-times "vaddph\[ \\t\]+\[^\n\r\]*%ymm\[01\]" 1 } } */
/* { dg-final { scan-assembler-times "vsubph\[ \\t\]+\[^\n\r\]*%xmm\[01\]" 1 } } */
/* { dg-final { scan-assembler-times "vsubph\[ \\t\]+\[^\n\r\]*%ymm\[01\]" 1 } } */
/* { dg-final { scan-assembler-times "vmulph\[ \\t\]+\[^\n\r\]*%xmm\[01\]" 1 } } */
/* { dg-final { scan-assembler-times "vmulph\[ \\t\]+\[^\n\r\]*%ymm\[01\]" 1 } } */
/* { dg-final { scan-assembler-times "vdivph\[ \\t\]+\[^\n\r\]*%xmm\[01\]" 1 } } */
/* { dg-final { scan-assembler-times "vdivph\[ \\t\]+\[^\n\r\]*%ymm\[01\]" 1 } } */
