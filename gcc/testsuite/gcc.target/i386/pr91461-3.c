/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx512f -mavx512vl" } */
/* { dg-final { scan-assembler-not "\tvmovaps\t" } } */
/* { dg-final { scan-assembler-not "\tvmovups\t" } } */

#include <immintrin.h>

void
foo1 (__m128i *p, __m128i a)
{
  register __m128i x __asm ("xmm16") = a;
  asm volatile ("" : "+v" (x));
  *p = x;
}

void
foo2 (__m128d *p, __m128d a)
{
  register __m128d x __asm ("xmm16") = a;
  asm volatile ("" : "+v" (x));
  *p = x;
}

void
foo3 (__float128 *p, __float128 a)
{
  register __float128 x __asm ("xmm16") = a;
  asm volatile ("" : "+v" (x));
  *p = x;
}

void
foo4 (__m128i_u *p, __m128i a)
{
  register __m128i x __asm ("xmm16") = a;
  asm volatile ("" : "+v" (x));
  *p = x;
}

void
foo5 (__m128d_u *p, __m128d a)
{
  register __m128d x __asm ("xmm16") = a;
  asm volatile ("" : "+v" (x));
  *p = x;
}

typedef __float128 __float128_u __attribute__ ((__aligned__ (1)));

void
foo6 (__float128_u *p, __float128 a)
{
  register __float128 x __asm ("xmm16") = a;
  asm volatile ("" : "+v" (x));
  *p = x;
}

typedef __int128 __int128_u __attribute__ ((__aligned__ (1)));

extern __int128 int128;

void
foo7 (__int128 *p)
{
  register __int128 x __asm ("xmm16") = int128;
  asm volatile ("" : "+v" (x));
  *p = x;
}

void
foo8 (__int128_u *p)
{
  register __int128 x __asm ("xmm16") = int128;
  asm volatile ("" : "+v" (x));
  *p = x;
}
