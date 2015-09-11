/* { dg-do run } */
/* { dg-shouldfail "bounds violation" } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#define SHOULDFAIL

#include "mpx-check.h"

int buf[100];
int *p = buf;

int mpx_test (int argc, const char *argv[])
{
  printf ("%d\n", p[-1]);

  return 0;
}
