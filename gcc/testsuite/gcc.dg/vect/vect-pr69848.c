/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_condition } */
#include <stdarg.h>
#include "tree-vect.h"

#define N 256
int a[N] = {0};

__attribute__ ((noinline))
int foo ()
{
  int i, res = 0;
  for (i = 0; i < N; i++)
  {
    if (a[i] != 0)
      res = 1;
  }
  return res;
}

int main (void)
{
  int i, res;

  check_vect ();

  if ((res = foo ()) != 0)
    abort ();

  a[34] = 101;
  a[85] = 9;
  if ((res = foo ()) != 1)
    abort ();

  return 0;
}
/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { xfail { ! vect_max_reduc } } } } */
