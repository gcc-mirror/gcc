/* { dg-do compile } */
/* { dg-options "-O2 -mavx2" } */

typedef long long __m256i __attribute__ ((__vector_size__ (32)));

int
foo (__m256i x, __m256i y)
{
  __m256i a = x & y;
  return __builtin_ia32_ptestc256 (a, a);
}

/* { dg-final { scan-assembler "movl\[ \\t]*\\\$1, %eax" } } */
