/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include "mpx-check.h"

int buf[100];

int mpx_test (int argc, const char **argv)
{
  int *p = __bnd_set_ptr_bounds (buf + 10, sizeof (int) * 10);
  int *p1 = __bnd_copy_ptr_bounds (buf, p);
  p1[10] = argc;
  p1[19] = argc;
  return 0;
}
