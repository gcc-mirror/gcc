/* { dg-do compile } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include "mpx-check.h"

void *p;
int mpx_test (int argc, const char **argv)
{
  p = &&label;

 label:
  return 0;
}
