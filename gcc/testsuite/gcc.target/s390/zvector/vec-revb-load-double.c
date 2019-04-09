/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=arch13 -mzvector" } */

#include <vecintrin.h>

vector double
test (vector double x)
{
  return vec_revb (x);
}

/* { dg-final { scan-assembler-times "vperm\t" 1 } } */


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

/* { dg-final { scan-assembler-times "vlbrg\t" 2 } } */
