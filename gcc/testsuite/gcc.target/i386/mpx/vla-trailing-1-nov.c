/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -fchkp-flexible-struct-trailing-arrays" } */


#include "mpx-check.h"

struct S
{
  int a;
  int p[10];
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
  rd (s->p, 0);
  rd (s->p, 99);
  s->p[0];
  s->p[99];
  
  return 0;
}
