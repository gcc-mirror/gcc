/* { dg-do compile } */
/* { dg-options "-O0 -mavx -mno-vzeroupper -dp" } */

#include <immintrin.h>

extern __m256 x, y;

void
foo ()
{
  x = y;
}

/* { dg-final { scan-assembler-not "avx_vzeroupper" } } */
