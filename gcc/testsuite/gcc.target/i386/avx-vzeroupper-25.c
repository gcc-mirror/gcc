/* { dg-do compile } */
/* { dg-options "-O0 -mavx -mtune=generic -dp" } */

#include <immintrin.h>

extern __m256 x, y;

void
foo ()
{
  x = y;
}

/* { dg-final { scan-assembler-not "avx_vzeroupper" } } */
