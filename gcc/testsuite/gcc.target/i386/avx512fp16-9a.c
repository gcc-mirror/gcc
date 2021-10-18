/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16" } */

#include <immintrin.h>

__m128h
__attribute__ ((noinline, noclone))
set1_128 (_Float16 x)
{
  return _mm_set1_ph (x);
}

__m256h
__attribute__ ((noinline, noclone))
set1_256 (_Float16 x)
{
  return _mm256_set1_ph (x);
}

__m512h
__attribute__ ((noinline, noclone))
set1_512 (_Float16 x)
{
  return _mm512_set1_ph (x);
}

/* { dg-final { scan-assembler-times "vpbroadcastw\[ \t]\+\[^\n\r]*\[xyz\]mm0" 3 } } */
