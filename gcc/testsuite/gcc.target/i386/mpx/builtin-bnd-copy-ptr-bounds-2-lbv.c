/* { dg-do run } */
/* { dg-shouldfail "bounds violation" } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#define SHOULDFAIL

#include "mpx-check.h"

int buf[100];

int mpx_test (int argc, const char **argv)
{
  int *p = __bnd_set_ptr_bounds (buf + 10, sizeof (int) * 10);
  int *p1 = __bnd_copy_ptr_bounds (buf, p);
  p1[9] = argc;
  return 0;
}
