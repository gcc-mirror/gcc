/* { dg-do run } */
/* { dg-shouldfail "bounds violation" } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#define SHOULDFAIL

#include "mpx-check.h"

struct s1
{
  int *p;
  int i1;
  int i2;
} s1;

int rd (int *p1, int *p2, int *p3, struct s1 s)
{
  int res = s.p[s.i1 + s.i2];
  printf ("%d\n", res);
  return res;
}

int buf[100];
int buf1[10];

int mpx_test (int argc, const char **argv)
{
  struct s1 s;
  s.p = buf;
  s.i1 = 50;
  s.i2 = 50;

  rd (buf1, buf1, buf1, s);

  return 0;
}
