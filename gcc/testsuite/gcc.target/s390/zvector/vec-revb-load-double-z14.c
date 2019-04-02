/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z14 -mzvector" } */

#include <vecintrin.h>

vector double
test (vector double x)
{
  return vec_revb (x);
}

vector double
test2 (vector double *x)
{
  return vec_revb (*x);
}

vector double
test3 (double *x)
{
  return vec_revb (vec_xl (0, x));
}

/* { dg-final { scan-assembler-times "vperm\t" 3 } } */
