/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include "mpx-check.h"

int buf[100];

int mpx_test (int argc, const char **argv)
{
  int *p = __bnd_narrow_ptr_bounds (buf + 10, buf, sizeof (int) * 100);
  p[0] = argc;
  p[89] = argc;
  return 0;
}
