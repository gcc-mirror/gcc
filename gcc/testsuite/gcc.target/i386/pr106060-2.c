/* { dg-do compile } */
/* { dg-options "-O3 -march=x86-64-v3" } */
#include <immintrin.h>

__m256i
foo ()
{
  /* should_be_cmpeq_abs */
  return _mm256_set1_epi8 (1);
}

/* { dg-final { scan-assembler "pcmpeq" } } */
/* { dg-final { scan-assembler "pabsb" } } */
