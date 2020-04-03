/* { dg-do compile } */
/* { dg-options "-O2 -mavx" } */
/* { dg-final { scan-assembler "\tvmovdqa\t" } } */
/* { dg-final { scan-assembler "\tvmovapd\t" } } */
/* { dg-final { scan-assembler-not "\tvmovaps\t" } } */

#include <immintrin.h>

void
foo1 (__m256i *p, __m256i x)
{
  *p = x;
}

void
foo2 (__m256d *p, __m256d x)
{
  *p = x;
}
