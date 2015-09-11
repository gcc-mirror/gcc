/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include "mpx-check.h"

int buf[100];
int *p;

int mpx_test (int argc, const char **argv)
{
  int *p1 = __bnd_set_ptr_bounds (buf + 10, sizeof (int) * 10);
  p = buf;
  __bnd_store_ptr_bounds ((void **)&p, p1 - 10);
  p[10] = argc;
  p[19] = argc;
  return 0;
}
