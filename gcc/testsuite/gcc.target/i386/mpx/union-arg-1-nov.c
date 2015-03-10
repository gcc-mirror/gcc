/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include "mpx-check.h"

struct s1
{
  union {
    int i1;
    int i3;
  } v;
  int i2;
  union {
    int *p;
    int p2;
  } u;
} s1;

int rd (struct s1 s)
{
  int res = s.u.p[s.v.i1 + s.i2];
  printf ("%d\n", res);
  return res;
}

int buf[100];
int buf1[10];

int mpx_test (int argc, const char **argv)
{
  struct s1 s;
  s.u.p = buf;
  s.v.i1 = 50;
  s.i2 = -50;

  rd (s);

  s.v.i1 = 50;
  s.i2 = 49;

  rd (s);

  return 0;
}
