/* { dg-do run } */
/* { dg-shouldfail "bounds violation" } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#define SHOULDFAIL

#include "mpx-check.h"

int mpx_test (int argc, const char **argv)
{
  int a[100];

  void rd (int i)
  {
    printf ("%d\n", a[i]);
  }

  rd (100);

  return 0;
}
