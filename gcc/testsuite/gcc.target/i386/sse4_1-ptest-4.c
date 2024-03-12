/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1" } */

typedef long long __m128i __attribute__ ((__vector_size__ (16)));

int foo (__m128i x, __m128i y)
{
  __m128i a = x & ~y;
  return __builtin_ia32_ptestz128 (a, a);
}

int bar (__m128i x, __m128i y)
{
  __m128i a = ~x & y;
  return __builtin_ia32_ptestz128 (a, a);
}

/* { dg-final { scan-assembler-times "ptest\[ \\t\]+%" 2 } } */
/* { dg-final { scan-assembler-times "setc" 2 } } */
/* { dg-final { scan-assembler-not "pandn" } } */
/* { dg-final { scan-assembler-not "sete" } } */

