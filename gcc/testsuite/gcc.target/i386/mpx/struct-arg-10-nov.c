/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include "mpx-check.h"

struct s1
{
  int *p;
} s1;

int rd (int *p1, int *p2, int *p3, int *p4, int *p5, int *p6, struct s1 s, int i)
{
  int res = s.p[i];
  printf ("%d\n", res);
  return res;
}

int buf[100];
int buf1[10];

int mpx_test (int argc, const char **argv)
{
  struct s1 s;
  s.p = buf;

  rd (buf1, buf1, buf1, buf1, buf1, buf1, s, 0);
  rd (buf1, buf1, buf1, buf1, buf1, buf1, s, 99);

  return 0;
}
