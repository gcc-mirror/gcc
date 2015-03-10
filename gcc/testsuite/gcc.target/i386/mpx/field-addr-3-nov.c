/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include "mpx-check.h"

struct S {
  int a;
  int b[100];
  int c;
} S;

int foo (int *p, int i)
{
  printf ("%d\n", p[i]);
  return p[i];
}

struct S s;

int mpx_test (int argc, const char **argv)
{
  foo (&s.b[0], 0);
  foo (&s.b[0], 99);

  return 0;
}
