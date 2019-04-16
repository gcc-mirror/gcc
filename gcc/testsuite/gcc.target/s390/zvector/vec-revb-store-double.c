/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=arch13 -mzvector" } */

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

/* { dg-final { scan-assembler-times "vstbrg\t" 2 } } */

/* mem -> mem: This becomes vlbrg + vst */
void
test2 (vector double *target, vector double *x)
{
  *target = vec_revb (*x);
}

/* { dg-final { scan-assembler-times "vlbrg\t" 1 } } */
