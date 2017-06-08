/* { dg-do run } */
/* { dg-shouldfail "bounds violation" } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#define SHOULDFAIL

#include "mpx-check.h"

typedef int v16 __attribute__((vector_size(16)));

int foo(int i) {
  register v16 u asm("xmm0");
  return u[i];
}

int mpx_test (int argc, const char **argv)
{
  printf ("%d\n", foo (-1));
  return 0;
}
