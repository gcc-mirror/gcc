/* { dg-do compile } */
/* { dg-options "-O3 -march=x86-64-v3" } */
#include <immintrin.h>

__m256i
foo ()
{
  /* shouldnt_have_movabs */
  return _mm256_set1_epi8 (123);
}

/* { dg-final { scan-assembler-not "movabs" } } */
