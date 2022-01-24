/* { dg-do compile} */
/* { dg-options "-O2 -mavx512fp16" } */

/* { dg-final { scan-assembler-times "vpxord\[ \\t\]+\[^\n\r\]*%zmm0" 1 } } */
/* { dg-final { scan-assembler-times "vxorps\[ \\t\]+\[^\n\r\]*%xmm0" 1 } } */

#include<immintrin.h>

_Float16
neghf (_Float16 a)
{
  return -a;
}

__m512h
neghf512 (__m512h a)
{
  return -a;
}
