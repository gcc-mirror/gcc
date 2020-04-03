/* PR target/91824 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx2" } */
/* { dg-final { scan-assembler-not "cltq" } } */
/* { dg-final { scan-assembler-not "movl\t%eax, %eax" } } */

#include <x86intrin.h>

unsigned long long
f1 (__m128i x)
{
  return _mm_movemask_epi8 (x);
}

unsigned long long
f2 (__m128i x)
{
  return (unsigned) _mm_movemask_epi8 (x);
}

unsigned long long
f3 (__m128 x)
{
  return _mm_movemask_ps (x);
}

unsigned long long
f4 (__m128 x)
{
  return (unsigned) _mm_movemask_ps (x);
}

unsigned long long
f5 (__m128d x)
{
  return _mm_movemask_pd (x);
}

unsigned long long
f6 (__m128d x)
{
  return (unsigned) _mm_movemask_pd (x);
}

unsigned long long
f7 (__m256 x)
{
  return _mm256_movemask_ps (x);
}

unsigned long long
f8 (__m256 x)
{
  return (unsigned) _mm256_movemask_ps (x);
}

unsigned long long
f9 (__m256d x)
{
  return _mm256_movemask_pd (x);
}

unsigned long long
f10 (__m256d x)
{
  return (unsigned) _mm256_movemask_pd (x);
}

unsigned long long
f11 (__m256i x)
{
  return (unsigned) _mm256_movemask_epi8 (x);
}
