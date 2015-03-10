/* { dg-do run } */
/* { dg-shouldfail "bounds violation" } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#define SHOULDFAIL

#include "mpx-check.h"

struct s1 {
  int *p[2];
} s1;

int rd (struct s1 s, int i)
{
  int res = s.p[0][i];
  printf ("%d\n", res);
  return res;
}

int buf[100];
int buf1[100];

int mpx_test (int argc, const char **argv)
{
  struct s1 s = { {buf, buf1} };

  rd (s, 100);

  return 0;
}
