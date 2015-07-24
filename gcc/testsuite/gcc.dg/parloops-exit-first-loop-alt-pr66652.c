/* { dg-do compile } */
/* { dg-require-effective-target pthread } */
/* { dg-options "-O2 -ftree-parallelize-loops=2 -fdump-tree-parloops-details" } */

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

/* { dg-final { scan-tree-dump-times "parallelizing inner loop" 1 "parloops" } } */
/* { dg-final { scan-tree-dump-times "alternative exit-first loop transform succeeded" 0 "parloops" } } */
