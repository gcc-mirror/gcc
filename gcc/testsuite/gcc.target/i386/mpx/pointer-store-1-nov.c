/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include "mpx-check.h"

int *buf1[100];
int buf2[100];

void wr (int i)
{
  buf1[i] = buf2;
}

int rd(int i, int j)
{
  int res = buf1[i][j];
  printf("%d\n", res);
  return res;
}

int mpx_test (int argc, const char **argv)
{
  wr(10);
  rd(10, 0);
  rd(10, 99);

  return 0;
}
