/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z14 -mzvector" } */

#include <vecintrin.h>

vector signed char
test (vector signed char x)
{
  return vec_reve (x);
}

vector signed char
test2 (vector signed char *x)
{
  return vec_reve (*x);
}

vector signed char
test3 (signed char *x)
{
  return vec_reve (vec_xl (0, x));
}

/* { dg-final { scan-assembler-times "vperm\t" 3 } } */
