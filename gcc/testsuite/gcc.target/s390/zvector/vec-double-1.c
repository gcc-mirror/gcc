/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z13 -mzvector" } */

#include <vecintrin.h>

vector double
test (vector unsigned long long x)
{
  return vec_double (x);
}

/* { dg-final { scan-assembler-times "vcdlgb\t" 1 } } */
