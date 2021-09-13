/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mtune=generic" } */

#include <immintrin.h>

extern __m512d y, z;

void
pr82941 ()
{
  z = y;
}

/* { dg-final { scan-assembler-times "vzeroupper" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-not "vzeroupper" { target { ! ia32 } } } } */
