/* { dg-do run { target i?86-*-* } } */
/* { dg-options "-march=pentium4" } */

#include <xmmintrin.h>
#include <stdio.h>
#include "../../gcc.dg/i386-cpuid.h"

int main(int argc, char** argv) {
  float a = 1.0f;
  float b = 2.0f;
  float c = 3.0f;
  float r;

  unsigned long cpu_facilities;

  cpu_facilities = i386_cpuid ();

  if ((cpu_facilities & (bit_MMX | bit_SSE | bit_SSE2 | bit_CMOV))
      != (bit_MMX | bit_SSE | bit_SSE2 | bit_CMOV))
    /* If host has no vector support, pass.  */
    return 0;

  __m128 v = _mm_set_ps(a, b, c, 0);
  
  v = (__m128)_mm_srli_si128((__m128i)v, 4);
  _mm_store_ss(&r, v);
  if (r != 3.0f)
    abort ();
  exit (0);
}
