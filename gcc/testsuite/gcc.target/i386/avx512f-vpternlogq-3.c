/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v4" } */

#include <immintrin.h>
__m256i
foo2 (__m256i** a, __m256i b)
{
  return ~(**a);
}
