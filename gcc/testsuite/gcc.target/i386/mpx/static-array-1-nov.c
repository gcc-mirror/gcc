/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include "mpx-check.h"

int buf[100];

int mpx_test (int argc, const char **argv)
{
  printf("%d\n", buf[0]);
  printf("%d\n", buf[99]);
  return 0;
}
