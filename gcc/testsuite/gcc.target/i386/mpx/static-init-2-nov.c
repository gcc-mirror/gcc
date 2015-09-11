/* { dg-do run } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#include "mpx-check.h"

struct s {
  int a;
  int *p;
} s;

int buf[100];

struct s s1 = {0, buf};

int mpx_test (int argc, const char *argv[])
{
  printf ("%d\n", s1.p[0]);
  printf ("%d\n", s1.p[99]);

  return 0;
}
