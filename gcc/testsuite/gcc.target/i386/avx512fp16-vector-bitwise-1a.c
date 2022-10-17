/* { dg-do compile } */
/* { dg-options "-Ofast -mavx512vl -mavx512fp16 -mprefer-vector-width=512" } */

#include<immintrin.h>
__m128h
f1 (__m128h x)
{
  int i = 0;
  __m128h y;
  for (; i != 8; i++)
    y[i] = __builtin_fabsf16 (x[i]);
  return y;
}

__m256h
f2 (__m256h x)
{
  int i = 0;
  __m256h y;
  for (; i != 16; i++)
    y[i] = __builtin_fabsf16 (x[i]);
  return y;
}

__m512h
f3 (__m512h x)
{
  int i = 0;
  __m512h y;
  for (; i != 32; i++)
    y[i] = __builtin_fabsf16 (x[i]);
  return y;
}

__m128h
f4 (__m128h x)
{
  return -x;
}

__m256h
f5 (__m256h x)
{
  return -x;
}

__m512h
f6 (__m512h x)
{
  return -x;
}

__m128h
f7 (__m128h x, __m128h y)
{
  int i = 0;
  __m128h z;
  for (; i != 8; i++)
    z[i] = __builtin_copysignf16 (x[i], y[i]);
  return z;
}

__m256h
f8 (__m256h x, __m256h y)
{
  int i = 0;
  __m256h z;
  for (; i != 16; i++)
    z[i] = __builtin_copysignf16 (x[i], y[i]);
  return z;
}

__m512h
f9 (__m512h x, __m512h y)
{
  int i = 0;
  __m512h z;
  for (; i != 32; i++)
    z[i] = __builtin_copysignf16 (x[i], y[i]);
  return z;
}

__m128h
f10 (__m128h x, __m128h y)
{
  int i = 0;
  __m128h z;
  for (; i != 8; i++)
    z[i] = x[i] * __builtin_copysignf16 (1, y[i]);
  return z;
}

__m256h
f11 (__m256h x, __m256h y)
{
  int i = 0;
  __m256h z;
  for (; i != 16; i++)
    z[i] = x[i] * __builtin_copysignf16 (1, y[i]);
  return z;
}

__m512h
f12 (__m512h x, __m512h y)
{
  int i = 0;
  __m512h z;
  for (; i != 32; i++)
    z[i] = x[i] * __builtin_copysignf16 (1, y[i]);
  return z;
}

/* { dg-final { scan-assembler "vandps\[^\n\r\]*xmm0" } } */
/* { dg-final { scan-assembler "vandps\[^\n\r\]*ymm0" } } */
/* { dg-final { scan-assembler "vpandd\[^\n\r\]*zmm0" } } */
/* { dg-final { scan-assembler-times "vxorps\[^\n\r\]*xmm0" 1 } } */
/* { dg-final { scan-assembler-times "vxorps\[^\n\r\]*ymm0" 1 } } */
/* { dg-final { scan-assembler-times "vpxord\[^\n\r\]*zmm0" 1 } } */
/* { dg-final { scan-assembler-times "vpternlogd\[^\n\r\]*xmm\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "vpternlogd\[^\n\r\]*ymm\[0-9\]" 2 } } */
/* { dg-final { scan-assembler-times "vpternlogd\[^\n\r\]*zmm\[0-9\]" 2 } } */
