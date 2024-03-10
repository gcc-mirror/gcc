/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1" } */

typedef long long __m128i __attribute__ ((__vector_size__ (16)));

int
foo (__m128i x, __m128i y)
{
  __m128i a = x & y;
  return __builtin_ia32_ptestc128 (a, a);
}

/* { dg-final { scan-assembler "movl\[ \\t]*\\\$1, %eax" } } */
