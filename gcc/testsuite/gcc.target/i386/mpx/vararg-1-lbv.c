/* { dg-do run } */
/* { dg-shouldfail "bounds violation" } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#define SHOULDFAIL

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
    va_arg (argp, int *);
  p = va_arg (argp, int *);
  i = va_arg (argp, int);

  res = p[i];
  printf ("%d\n", res);

  return res;
}

int mpx_test (int argc, const char **argv)
{
  rd (buf1, 0, buf, -1);
  return 0;
}
