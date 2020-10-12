/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

extern void abort (void);

int a[8], b[8];

void __attribute__((noinline,noclone))
foo(void)
{
  a[0] = b[0];
  a[1] = b[0];
  a[2] = b[3];
  a[3] = b[3];
  a[4] = b[4];
  a[5] = b[7];
  a[6] = b[4];
  a[7] = b[7];
}

int main()
{
  int i;
  check_vect ();
  for (i = 0; i < 8; ++i)
    b[i] = i;
  foo ();
  if (a[0] != 0 || a[1] != 0 || a[2] != 3 || a[3] != 3
      || a[4] != 4 || a[5] != 7 || a[6] != 4 || a[7] != 7)
    abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "optimized: basic block" "slp2" { target vect_perm } } } */
