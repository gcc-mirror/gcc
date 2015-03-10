/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include "mpx-check.h"

int buf[100];

int rd (int t1, int t2, int t3, int t4, int t5, int *p, int i)
{
  int res = p[i];
  printf ("%d\n", res);
  return res;
}

int mpx_test (int argc, const char **argv)
{
  int *p;

  rd (0, 0, 0, 0, 0, buf, 0);
  rd (0, 0, 0, 0, 0, buf, 99);

  return 0;
}
