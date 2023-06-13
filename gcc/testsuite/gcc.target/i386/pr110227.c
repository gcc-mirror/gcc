/* { dg-do assemble { target { ! ia32 } } } */
/* { dg-options " -O2 -mavx512vl" } */

#include <immintrin.h>

void f()
{
  __m256i mask = _mm256_set_epi32(0, 0, 0, 0, -1, -1, -1, -1);
  register __m256i reg asm("xmm16") = mask;
  asm(""::"v"(reg));
}
