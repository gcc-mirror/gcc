/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z14 -mzvector" } */

#include <vecintrin.h>

/* reg -> mem */
void
test (vector double *target, vector double x)
{
  *target = vec_revb (x);
}

void
test3 (double *target, vector double x)
{
  vec_xst (vec_revb (x), 0, target);
}

/* mem -> mem */
void
test2 (vector double *target, vector double *x)
{
  *target = vec_revb (*x);
}

/* { dg-final { scan-assembler-times "vperm\t" 3 } } */
