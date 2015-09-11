/* { dg-do run } */
/* { dg-shouldfail "bounds violation" } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */

/* { dg-additional-options "-Wno-attributes" } */

#define SHOULDFAIL

#include "mpx-check.h"
#include <stdarg.h>

int
foo2 (int i1, int *p1, ...)
{
  va_list argp;
  int *p;
  int i;
  int res;

  va_start(argp, p1);
  p = va_arg(argp, int *);
  i = va_arg(argp, int);

  res = p[i + i1];
  printf("%d\n", res);

  return res;
}

static __attribute__((always_inline)) int
foo1 (int *p1, ...)
{
  return foo2 (10, p1, __va_arg_pack ());
}

int prebuf[100];
int buf[100];
int buf1[100];
int postbuf[100];

int mpx_test (int argc, const char **argv)
{
  foo1 (buf, buf1, 90);
  return 0;
}
