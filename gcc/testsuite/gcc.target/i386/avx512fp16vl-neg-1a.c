/* { dg-do compile} */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl" } */

/* { dg-final { scan-assembler-times "vxorps\[ \\t\]+\[^\n\r\]*%xmm0" 1 } } */
/* { dg-final { scan-assembler-times "vxorps\[ \\t\]+\[^\n\r\]*%ymm0" 1 } } */
#include<immintrin.h>

__m128h
neghf128 (__m128h a)
{
  return -a;
}

__m256h
neghf256 (__m256h a)
{
  return -a;
}
