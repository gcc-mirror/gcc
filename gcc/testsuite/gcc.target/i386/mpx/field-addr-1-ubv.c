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

int foo (int *i, int k)
{
  printf ("%d\n", i[k]);
  return i[k];
}

int mpx_test (int argc, const char **argv)
{
  struct S s;

  foo(&s.a, 102);

  return 0;
}
