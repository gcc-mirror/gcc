/* { dg-do run } */
/* { dg-shouldfail "bounds violation" } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#define SHOULDFAIL

#include "mpx-check.h"

int *_buf1[100];
int _buf2[100];

int **get_buf1 ()
{
  return _buf1;
}

int *get_buf2 ()
{
  return _buf2;
}

void wr (int i, int **buf1, int *buf2)
{
  buf1[i] = buf2;
}

int rd (int i, int j, int **buf)
{
  int res = buf[i][j];
  printf ("%d\n", res);
  return res;
}

int mpx_test (int argc, const char **argv)
{
  int **buf1 = get_buf1 ();
  int *buf2 = get_buf2 ();
  wr(10, buf1, buf2);
  rd(10, 100, buf1);

  return 0;
}
