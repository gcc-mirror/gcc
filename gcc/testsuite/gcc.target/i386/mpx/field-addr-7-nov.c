/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include "mpx-check.h"

struct S {
  int a[10];
  int b;
} S;

struct S1 {
  int a;
  struct S b[10];
  int c;
} S1;

struct S2 {
  int x;
  struct S1 a[10];
  struct S1 b;
} S2;

int foo (int *p, int i)
{
  int res = p[i];
  printf ("%d\n", res);
  return res;
}

struct S2 s1;
struct S2 *s2 = &s1;

int mpx_test (int argc, const char **argv)
{
  foo (&(s2->a[0].a), 0);
  foo (&(s2->a[9].c), 0);

  return 0;
}
