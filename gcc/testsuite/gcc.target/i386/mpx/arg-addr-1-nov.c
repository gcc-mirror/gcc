/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include "mpx-check.h"

int rd (int *p, int i)
{
  int res = p[i];
  printf ("%d\n", res);
  return res;
}

int foo (int i, int j)
{
  return rd(&i, j);
}

int mpx_test (int argc, const char **argv)
{
  foo (1, 0);

  return 0;
}
