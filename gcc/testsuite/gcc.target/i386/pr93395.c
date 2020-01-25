/* PR target/93395 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -masm=att" } */
/* { dg-final { scan-assembler-times "vpermilpd\t.5, %ymm" 3 } } */
/* { dg-final { scan-assembler-times "vpermilpd\t.85, %zmm" 3 } } */
/* { dg-final { scan-assembler-not "vpermpd\t" } } */

#include <immintrin.h>

__m256d
foo1 (__m256d a)
{
  return _mm256_permute4x64_pd (a, 177);
}

__m256d
foo2 (__m256d a)
{
  return _mm256_permute_pd (a, 5);
}

__m256d
foo3 (__m256d a)
{
  return __builtin_shuffle (a, (__v4di) { 1, 0, 3, 2 });
}

__m512d
foo4 (__m512d a)
{
  return _mm512_permutex_pd (a, 177);
}

__m512d
foo5 (__m512d a)
{
  return _mm512_permute_pd (a, 85);
}

__m512d
foo6 (__m512d a)
{
  return __builtin_shuffle (a, (__v8di) { 1, 0, 3, 2, 5, 4, 7, 6 });
}
