/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include "mpx-check.h"

int buf[100];

int __attribute((nothrow))
rd (int *&p, int i)
{
  int res = p[i];
  printf ("%d\n", res);
  return res;
}

void set_p (int *&ref)
{
  ref = buf;
}

int mpx_test (int argc, const char **argv)
{
  int *p;

  set_p (p);
  rd (p, 0);
  rd (p, 99);

  return 0;
}
