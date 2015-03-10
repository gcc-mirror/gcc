/* { dg-do run } */
/* { dg-shouldfail "bounds violation" } */
/* { dg-options "-fcheck-pointer-bounds -mmpx" } */


#define SHOULDFAIL

#include "mpx-check.h"

int buf1[100];
int buf2[200];

struct s1 {
  int a;
  int *p[2];
} s1;

struct s2 {
  int a;
  struct s1 b[2];
} s2;

struct s2 s = { 1, { {1, { buf1, buf2 }}, {2, { buf2, buf1} } } };

int mpx_test (int argc, const char **argv)
{
  printf ("%d\n", s.b[0].p[0][-1]);

  return 0;
}
