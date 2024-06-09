/* PR target/112435 */
/* { dg-do assemble { target { avx512vl && { ! ia32 } } } } */
/* { dg-options "-mavx512vl -O2" } */

#include <x86intrin.h>

__m256i
foo (__m256i a, __m256i b)
{
  register __m256i c __asm__("ymm16") = a;
  asm ("" : "+v" (c));
  return _mm256_shuffle_i32x4 (c, b, 2);
}
