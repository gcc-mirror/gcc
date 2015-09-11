/* { dg-require-effective-target vect_int_mult } */

#include <stdarg.h>
#include "tree-vect.h"

#define N 10
#define RES 1024

/* A reduction pattern in which there is no data ref in
   the loop and one operand is defined outside of the loop.  */

__attribute__ ((noinline)) int
foo (int v)
{
  int i;
  int result = 1;

  ++v;
  for (i = 0; i < N; i++)
    result *= v;

  return result;
}

int
main (void)
{
  int res;

  check_vect ();

  res = foo (1);
  if (res != RES)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */

