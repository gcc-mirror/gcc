/* { dg-do run } */
/* { dg-options "-fno-check-pointer-bounds" } */


#include "mpx-check.h"

int buf[100];
int *p;

int mpx_test (int argc, const char **argv)
{
  int *p1 = __bnd_set_ptr_bounds (buf + 10, sizeof (int) * 10);
  p = buf;
  __bnd_store_ptr_bounds ((void **)&p, p1);
  return 0;
}
