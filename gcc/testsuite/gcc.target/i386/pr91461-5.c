/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-final { scan-assembler-not "\tvmovaps\t" } } */

#include <immintrin.h>

void
foo1 (__m512i *p, __m512i x)
{
  *p = x;
}

void
foo2 (__m512d *p, __m512d x)
{
  *p = x;
}
