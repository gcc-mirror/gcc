/* { dg-do compile } */
/* { dg-options "-O2 -mavx -mtune=generic -dp" } */

#include <immintrin.h>

extern __m256 x, y;

void
foo ()
{
  x = y;
}

/* { dg-final { scan-assembler-times "avx_vzeroupper" 1 } } */
