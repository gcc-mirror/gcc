/* Test case to check if AVX intrinsics and function specific target
   optimizations work together.  Check by including immintrin.h  */

/* { dg-do compile } */
/* { dg-options "-O2 -msse -mno-avx" } */

#include <immintrin.h>

__m256 a[10], b[10], c[10];
void __attribute__((target ("avx")))
foo (void)
{
  a[0] = _mm256_and_ps (b[0], c[0]);
}
