/* { dg-do run } */
/* { dg-shouldfail "bounds violation" } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#define SHOULDFAIL

#include "mpx-check.h"

__attribute__((fastcall)) int rd (int *p1, int *p2, int i)
{
  int res = p2[i];
  printf ("%d\n", res);
  return res;
}

int buf[100];
int buf1[10];

int mpx_test (int argc, const char **argv)
{
  rd (buf, buf1, -1);

  return 0;
}
