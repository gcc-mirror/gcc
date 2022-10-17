/* PR c++/101180 */
/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -mno-crc32" } */

#include <x86intrin.h>

__attribute__((target ("avx"))) __attribute__((target ("crc32"))) void
foo (__m256 *p, unsigned int *q)
{
  __m256 c = _mm256_and_ps (p[0], p[1]);
  *q = __crc32b (*q, 0x55);
}
