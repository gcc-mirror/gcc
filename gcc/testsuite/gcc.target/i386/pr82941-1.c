/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512" } */

#include <immintrin.h>

extern __m512d y, z;

void
pr82941 ()
{
  z = y;
}

/* { dg-final { scan-assembler-times "vzeroupper" 1 } } */
