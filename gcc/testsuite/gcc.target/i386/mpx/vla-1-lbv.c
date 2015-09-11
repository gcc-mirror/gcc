/* { dg-do run } */
/* { dg-shouldfail "bounds violation" } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#define SHOULDFAIL

#include "mpx-check.h"

struct S
{
  int a;
  int p[0];
};

int rd (int *p, int i)
{
  int res = p[i];
  printf ("%d\n", res);
  return res;
}

int mpx_test (int argc, const char **argv)
{
  struct S *s = (struct S *)alloca (sizeof(struct S) + sizeof (int)*100);
  rd (s->p, -2);

  return 0;
}
