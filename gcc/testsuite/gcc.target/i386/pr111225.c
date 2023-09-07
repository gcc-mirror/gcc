/* { dg-do compile } */
/* { dg-options "-O1 -fsanitize=thread -mforce-drap -mavx512cd" } */

typedef long long __m256i __attribute__ ((__vector_size__ (32)));

int foo (__m256i x, __m256i y)
{
  __m256i a = x & ~y;
  return !__builtin_ia32_ptestz256 (a, a);
}

int bar (__m256i x, __m256i y)
{
  __m256i a = ~x & y;
  return !__builtin_ia32_ptestz256 (a, a);
}
