/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

#include <immintrin.h>

__attribute__ ((target ("avx10.1-512,avx10.1-256"))) void
f1 ()
{ /* { dg-warning "The options used for AVX10 have conflict vector width, using the latter 256 as vector width" } */
  register __m256d a __asm ("ymm17");
  register __m256d b __asm ("ymm16");
  a = _mm256_add_pd (a, b);
  asm volatile ("" : "+v" (a));
}
