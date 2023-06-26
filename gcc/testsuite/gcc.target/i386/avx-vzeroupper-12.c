/* { dg-do compile } */
/* { dg-options "-O0 -mavx -mvzeroupper -dp" } */

#include <immintrin.h>

extern __m256 x, y;

void
foo ()
{
  _mm256_zeroall ();
  _mm256_zeroupper ();
  x = y;
  _mm256_zeroupper ();
  _mm256_zeroupper ();
  _mm256_zeroupper ();
}

/* { dg-final { scan-assembler-times "avx_vzeroupper" 4 { target ia32 } } } */
/* { dg-final { scan-assembler-times "avx_vzeroupper" 5 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "\\*avx_vzeroall" 1 } } */
