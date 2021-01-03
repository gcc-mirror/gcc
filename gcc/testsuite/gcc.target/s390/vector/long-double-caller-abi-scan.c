/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch" } */
#include <assert.h>

long double long_double_callee (long double x, int n, ...);

int
main ()
{
  assert (long_double_callee (1.L, 2, 2.L, 3.L) == 6.L);
}

/* { dg-final { scan-assembler-times {\n\tvst\t} 3 } } */
