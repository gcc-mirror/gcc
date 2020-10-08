/* { dg-require-effective-target vect_double } */

#include "tree-vect.h"

extern void abort (void);

double a[8], b[8];
int x;

void __attribute__((noinline,noclone))
bar (void)
{
  x = 1;
}

void __attribute__((noinline,noclone))
foo(int i)
{
  double tem1 = a[2*i];
  double tem2 = 2*a[2*i+1];
  bar ();
  b[2*i] = 2*tem1;
  b[2*i+1] = tem2;
}

int main()
{
  int i;
  check_vect ();
  for (i = 0; i < 8; ++i)
    b[i] = i;
  foo (2);
  return 0;
}

/* { dg-final { scan-tree-dump "optimized: basic block" "slp2" } } */
