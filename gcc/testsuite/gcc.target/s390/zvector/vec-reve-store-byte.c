/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=arch13 -mzvector" } */

#include <vecintrin.h>

/* reg -> mem */
void
foo (vector signed char *target, vector signed char x)
{
  *target = vec_reve (x);
}

void
bar (signed char *target, vector signed char x)
{
  vec_xst (vec_reve (x), 0, target);
}

/* { dg-final { scan-assembler-times "vstbrq\t" 2 } } */

/* mem -> mem: This becomes vlbrq + vst */
void
baz (vector signed char *target, vector signed char *x)
{
  *target = vec_reve (*x);
}

/* { dg-final { scan-assembler-times "vlbrq\t" 1 } } */
