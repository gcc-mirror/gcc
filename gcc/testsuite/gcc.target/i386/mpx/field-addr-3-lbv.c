/* { dg-do run } */
/* { dg-shouldfail "bounds violation" } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#define SHOULDFAIL

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
  foo (&s.b[0], -1);

  return 0;
}
