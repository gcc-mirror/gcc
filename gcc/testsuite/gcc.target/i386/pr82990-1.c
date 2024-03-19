/* { dg-do compile } */
/* { dg-options "-O2 -march=knl -mvzeroupper" } */
/* { dg-warning "'-march=knl' support will be removed in GCC 15" "" { target *-*-* } 0 } */

#include <immintrin.h>

extern __m512d y, z;

void
pr82941 ()
{
  z = y;
}

/* { dg-final { scan-assembler-times "vzeroupper" 1 } } */
