/* { dg-do run } */
/* { dg-shouldfail "bounds violation" } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#define SHOULDFAIL

#include "mpx-check.h"
#include <stdarg.h>

int buf[100];
int buf1[10];

void vararg (int *p, va_list al)
{
  int i;
  int res;

  p = va_arg (al, int *);
  i = va_arg (al, int);

  res = p[i];
  printf ("%d\n", res);
}

void foo (int *p, ...)
{
  va_list args;

  va_start (args, p);
  vararg (p, args);
  va_end (args);
}

int mpx_test (int argc, const char **argv)
{
  foo (buf1, buf, 100);

  return 0;
}
