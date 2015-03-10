/* { dg-do run } */
/* { dg-options "-fno-check-pointer-bounds" } */


#include "mpx-check.h"

int buf[100];

int mpx_test (int argc, const char **argv)
{
  __bnd_chk_ptr_bounds (buf - 1, sizeof (int));
  __bnd_chk_ptr_bounds (buf + 100, sizeof (int));
  return 0;
}
