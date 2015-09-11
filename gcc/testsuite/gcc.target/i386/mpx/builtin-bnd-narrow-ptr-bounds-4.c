/* { dg-do run } */
/* { dg-options "-fno-check-pointer-bounds" } */


#include "mpx-check.h"

int buf[100];

int mpx_test (int argc, const char **argv)
{
  assert (buf - 10 == __bnd_narrow_ptr_bounds (buf - 10, buf, 4));
  return 0;
}
