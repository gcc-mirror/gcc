/* { dg-do compile } */
/* { dg-options "-O0 -mavx -mvzeroupper -dp" } */

#include <immintrin.h>

extern __m256 x, y;

void
foo ()
{
  x = y;
  _mm256_zeroall ();
  _mm256_zeroupper ();
}

/* { dg-final { scan-assembler-times "avx_vzeroupper" 1 } } */
