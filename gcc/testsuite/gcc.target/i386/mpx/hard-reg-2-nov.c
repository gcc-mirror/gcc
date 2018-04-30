/* { dg-do run { target sse2_runtime } } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -msse2" } */

#include "mpx-check.h"

typedef int v16 __attribute__((vector_size(16)));

int foo (int i) {
  register v16 u asm ("xmm0");
  return u[i];
}

int mpx_test (int argc, const char **argv)
{
  printf ("%d\n", foo (3));
  printf ("%d\n", foo (0));
  return 0;
}
