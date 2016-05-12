/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512vl" } */

#include <x86intrin.h>

__m128
f1 (__m128 a, __m128 b)
{
  register __m128 c __asm ("xmm16") = a;
  asm volatile ("" : "+v" (c));
  c = _mm_insert_ps (c, b, 1);
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler "vinsertps\[^\n\r\]*xmm16" } } */

__v4sf
f2 (__v4sf a, float b)
{
  register __v4sf c __asm ("xmm17") = a;
  asm volatile ("" : "+v" (c));
  c[1] = b;
  asm volatile ("" : "+v" (c));
  return c;
}

/* { dg-final { scan-assembler "vinsertps\[^\n\r\]*xmm17" } } */

__v4sf
f3 (__v4sf a, float b)
{
  register float c __asm ("xmm18") = b;
  asm volatile ("" : "+v" (c));
  a[1] = c;
  return a;
}

/* { dg-final { scan-assembler "vinsertps\[^\n\r\]*xmm18" } } */
