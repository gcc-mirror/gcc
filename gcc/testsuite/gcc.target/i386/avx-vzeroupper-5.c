/* { dg-do compile } */
/* { dg-options "-O0 -mavx -mvzeroupper -dp" } */

#include <immintrin.h>

extern void bar2 (__m256);
extern __m256 y;

void
foo ()
{
  bar2 (y);
  _mm256_zeroupper ();
}

/* { dg-final { scan-assembler-not "avx_vzeroupper" } } */
