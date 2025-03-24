/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx512f" } */
/* { dg-require-ifunc "" } */

#include <immintrin.h>
__attribute__((target_clones ("default","avx10.1")))
__m512d foo(__m512d a, __m512d b)
{
  return a + b;
}
