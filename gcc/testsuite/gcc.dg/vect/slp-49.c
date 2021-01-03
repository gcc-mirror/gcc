/* { dg-require-effective-target vect_int } */

#include "tree-vect.h"

int a[1024];

void __attribute__((noipa))
foo(int k)
{
  int j = 5;
  for (int i = 0; i < 512; ++i)
    {
      a[2*i] = j;
      a[2*i+1] = k;
      j++;
      k+=3;
    }
}

int
main()
{
  check_vect ();

  foo (17);

  for (int i = 0; i < 512; ++i)
    {
      if (a[2*i] != 5 + i
	  || a[2*i+1] != 17 + 3 * i)
	__builtin_abort ();
    }

  return 0;
}

/* We don't yet support SLP inductions for variable length vectors.  */
/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" { xfail vect_variable_length } } } */
/* { dg-final { scan-tree-dump "Loop contains only SLP stmts" "vect" } } */
