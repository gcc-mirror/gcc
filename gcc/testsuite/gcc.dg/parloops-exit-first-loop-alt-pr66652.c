/* { dg-do compile } */
/* { dg-require-effective-target pthread } */
/* { dg-options "-O2 -ftree-parallelize-loops=2 -fdump-tree-parloops" } */

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

unsigned int
f (unsigned int n, unsigned int sum)
{
  unsigned int i;

  i = UINT_MAX;
  do
    {
      sum += i % 13;
      i++;
    }
  while (i < n - 1);

  return sum;
}

/* Four times % 13:
   - once in f._loopfn.0
   - once in the parallel
   - once in the low iteration count loop
   - once for a peeled off last iteration following the parallel.
   In other words, we want try_transform_to_exit_first_loop_alt to fail.  */
/* { dg-final { scan-tree-dump-times "(?n)% 13" 4 "parloops" } } */
