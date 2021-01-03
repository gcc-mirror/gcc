/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch -fno-unroll-loops" } */
#include <stdarg.h>

__attribute__ ((noipa, used)) long double
long_double_callee (long double x, int n, ...)
{
  long double sum = x;
  va_list vl;
  int i;

  va_start (vl, n);
  for (i = 0; i < n; i++)
    sum += va_arg (vl, long double);
  va_end (vl);

  return sum;
}

/* { dg-final { scan-assembler-times {\n\tvl\t} 3 } } */
