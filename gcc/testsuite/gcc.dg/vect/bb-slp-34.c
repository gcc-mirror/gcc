/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

extern void abort (void);

int a[8], b[8];

void __attribute__((noinline,noclone))
foo(void)
{
  a[0] = b[3];
  a[1] = b[2];
  a[2] = b[1];
  a[3] = b[0];
  a[4] = b[2];
  a[5] = b[3];
  a[6] = b[4];
  a[7] = b[5];
}

int main()
{
  int i;
  check_vect ();
  for (i = 0; i < 8; ++i)
    b[i] = i;
  foo ();
  if (a[0] != 3 || a[1] != 2 || a[2] != 1 || a[3] != 0
      || a[4] != 2 || a[5] != 3 || a[6] != 4 || a[7] != 5)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "basic block vectorized" "slp2" { target vect_perm } } } */
