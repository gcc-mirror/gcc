/* PR target/13685 */
/* { dg-do run } */
/* { dg-options "-Os -msse" } */
#include <stdlib.h>
#include <stdio.h>
#include <xmmintrin.h>
#include "../../gcc.dg/i386-cpuid.h"

void foo (__m128 *, __m64 *, int);

__m128 xmm0 = { 0 };
__m64 mm0 = { 0 };

int
main ()
{
  unsigned long cpu_facilities = i386_cpuid ();
  
  if ((cpu_facilities & (bit_MMX | bit_SSE | bit_CMOV))
      != (bit_MMX | bit_SSE | bit_CMOV))
    /* If host has no SSE support, pass.  */
    return 0;

  foo (&xmm0, &mm0, 4);
  return 0;
}

void
foo (__m128 *dst, __m64 *src, int n)
{
  __m128 xmm0 = { 0 };
  while (n > 64)
    {
      puts ("");
      xmm0 = _mm_cvtpi32_ps (xmm0, *src);
      *dst = xmm0;
      n--;
    }
}
