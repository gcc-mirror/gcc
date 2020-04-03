/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512" } */

#include <immintrin.h>

long long *p;
volatile __m256i y;

void
foo (void)
{
   _mm256_store_epi64 (p, y);
}

/* { dg-final { scan-assembler-not "vmovdqa64" } } */
