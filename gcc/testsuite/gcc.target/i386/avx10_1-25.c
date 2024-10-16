/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v2 -mavx" } */
/* { dg-require-ifunc "" } */

#include <immintrin.h>
__attribute__((target_clones ("default","avx10.1-256")))
__m256d foo(__m256d a, __m256d b)
{
  return a + b;
}
