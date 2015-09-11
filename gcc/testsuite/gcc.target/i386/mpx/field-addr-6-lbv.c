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

struct S1 {
  int x;
  struct S a[10];
  struct S b;
} S1;

int foo (int *p, int i)
{
  printf ("%d\n", p[i]);
  return p[i];
}

struct S1 s1;
struct S1 *s2 = &s1;

int mpx_test (int argc, const char **argv)
{
  foo (&(s2->a[0].a), -1);

  return 0;
}
