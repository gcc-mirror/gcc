/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=arch13 -mzvector" } */

#include <vecintrin.h>

vector signed short
foo (vector signed short x)
{
  return vec_reve (x);
}

/* { dg-final { scan-assembler-times "vpdi\t" 1 } } */
/* { dg-final { scan-assembler-times "verllg\t" 1 } } */
/* { dg-final { scan-assembler-times "verllf\t" 1 } } */


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

/* { dg-final { scan-assembler-times "vlerh\t" 2 } } */
