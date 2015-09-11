/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include "mpx-check.h"

int rd (int *p, int i)
{
  int res = p[i];
  printf ("%d\n", res);
  return res;
}

int mpx_test (int argc, const char **argv)
{
  int *buf = (int *)malloc (100 * sizeof(int));
  buf = (int *)realloc (buf, 200 * sizeof(int));

  rd (buf, 0);
  rd (buf, 199);

  free (buf);

  return 0;
}
