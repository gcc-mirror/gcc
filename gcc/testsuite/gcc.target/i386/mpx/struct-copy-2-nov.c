/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include "mpx-check.h"

struct S {
  int a;
  int b[100];
  int c;
} S;

int foo (void *p, int k)
{
  struct S *s = (struct S*)p;
  int res = s->b[k];
  printf ("%d\n", res);
  return res;
}

int mpx_test (int argc, const char **argv)
{
  struct S s;

  foo (&s.a, 0);
  foo (&s.a, 99);

  return 0;
}
