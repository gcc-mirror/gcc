/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include "mpx-check.h"

int rd (int *p, int i)
{
  int res = p[i];
  printf ("%d\n", res);
  return res;
}

int mpx_test (int argc, const char **argv)
{
#ifdef __x86_64__
  register int *frame __asm__("rsp");
#else
  register int *frame __asm__("esp");
#endif
  rd (frame, 1);

  return 0;
}
