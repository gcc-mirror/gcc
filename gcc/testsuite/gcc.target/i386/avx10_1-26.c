/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-ifunc "" } */

#include <immintrin.h>
__attribute__((target_clones ("default","avx10.1-512")))
__m512d foo(__m512d a, __m512d b)
{
  return a + b;
}
