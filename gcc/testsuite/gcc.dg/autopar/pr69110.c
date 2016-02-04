/* { dg-do compile } */
/* { dg-options "-O1 -ftree-parallelize-loops=2 -fno-tree-loop-im -fdump-tree-parloops2-details" } */

#define N 1000

unsigned int i = 0;

void
foo (void)
{
  unsigned int z;
  for (z = 0; z < N; ++z)
    ++i;
}

/* { dg-final { scan-tree-dump-times "SUCCESS: may be parallelized" 0 "parloops2" } } */
/* { dg-final { scan-tree-dump-times "FAILED: data dependencies exist across iterations" 1 "parloops2" } } */
