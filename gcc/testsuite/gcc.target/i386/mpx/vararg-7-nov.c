/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include "mpx-check.h"
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
  printf ("%d\n", res);

  return res;
}

int mpx_test (int argc, const char **argv)
{
  rd (buf1, 2, 10.0d, 10.0d, buf, 0, buf1);
  rd (buf1, 2, 10.0d, 10.0d, buf, 99, buf1);
  return 0;
}
