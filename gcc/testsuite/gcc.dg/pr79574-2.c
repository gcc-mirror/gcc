/* PR rtl-optimization/79574 */
/* { dg-do compile } */
/* { dg-options "-Os --param gcse-cost-distance-ratio=2147483647" } */

#include "stdarg.h"

int buf[100];
int buf1[10];

int rd (int *pppp, int n, ...)
{
  va_list argp;
  int *p;
  int i;
  int res;

  va_start (argp, n);
  for (; n > 0; n--)
    va_arg (argp, double);
  p = va_arg (argp, int *);
  i = va_arg (argp, int);

  res = p[i];
  __builtin_printf ("%d\n", res);

  return res;
}

int mpx_test (int argc, const char **argv)
{
  rd (buf1, 2, 10.0d, 10.0d, buf, 100, buf1);
  return 0;
}
