/* { dg-do run } */
/* { dg-shouldfail "bounds violation" } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#define SHOULDFAIL

#include "mpx-check.h"

struct s1
{
  int i1;
  int i2;
  int *p;
} s1;

int rd (struct s1 s)
{
  int res = s.p[s.i1 + s.i2];
  printf ("%d\n", res);
  return res;
}

int buf[100];

int mpx_test (int argc, const char **argv)
{
  struct s1 s;
  s.p = buf;
  s.i1 = 50;
  s.i2 = -51;

  rd (s);

  return 0;
}
