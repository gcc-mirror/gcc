/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512f -mavx512vl" } */
/* { dg-final { scan-assembler-not "\tvmovaps\t" } } */

#include <immintrin.h>

void
foo1 (__m256i *p, __m256i a)
{
  register __m256i x __asm ("xmm16") = a;
  asm volatile ("" : "+v" (x));
  *p = x;
}

void
foo2 (__m256d *p, __m256d a)
{
  register __m256d x __asm ("xmm16") = a;
  asm volatile ("" : "+v" (x));
  *p = x;
}
