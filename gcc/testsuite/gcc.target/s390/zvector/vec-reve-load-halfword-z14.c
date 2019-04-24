/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z14 -mzvector" } */

#include <vecintrin.h>

vector signed short
foo (vector signed short x)
{
  return vec_reve (x);
}

vector signed short
bar (vector signed short *x)
{
  return vec_reve (*x);
}

vector signed short
baz (signed short *x)
{
  return vec_reve (vec_xl (0, x));
}

/* { dg-final { scan-assembler-times "vperm\t" 3 } } */
