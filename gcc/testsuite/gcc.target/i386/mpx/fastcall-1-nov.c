/* { dg-do run } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include "mpx-check.h"

__attribute__((fastcall)) int rd (int *p1, int *p2, int i)
{
  int res = p1[i];
  printf ("%d\n", res);
  return res;
}

int buf[100];
int buf1[10];

int mpx_test (int argc, const char **argv)
{
  rd (buf1, buf, 0);
  rd (buf1, buf, 9);

  return 0;
}
