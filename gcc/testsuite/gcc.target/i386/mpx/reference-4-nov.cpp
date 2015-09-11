/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include "mpx-check.h"

int buf[100];

int rd (int (&p)[100], int i)
{
  int res = p[i];
  printf ("%d\n", res);
  return res;
}

int (&get_buf ()) [100]
{
  return buf;
}

int mpx_test (int argc, const char **argv)
{
  int *p;

  rd (get_buf (), 0);
  rd (get_buf (), 99);

  return 0;
}
