/* { dg-do run } */
/* { dg-options "-fno-check-pointer-bounds" } */


#include "mpx-check.h"

char buf[100];

int mpx_test (int argc, const char **argv)
{
  assert ((void *)0 == __bnd_get_ptr_lbound (buf));
  return 0;
}
