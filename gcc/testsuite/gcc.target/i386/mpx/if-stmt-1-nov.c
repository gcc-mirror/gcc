/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include "mpx-check.h"

int rd (int *p1, int *p2, int i)
{
  int res;

  if (p1 < p2)
    p1 = p2;

  res = p1[i];
  printf ("%d\n", res);
  return res;
}

int buf[100];

int mpx_test (int argc, const char **argv)
{
  int *p = __bnd_set_ptr_bounds (buf + 1, 40);
  rd (buf, p, 0);
  rd (buf, p, 9);

  return 0;
}
