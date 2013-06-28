/* This tests strength reduction and choice of induction variables.  The targets
   for this testcase are quite limited, as with different set of available
   addressing modes, the results may be quite different.
 
   The testcase comes from PR 29256 (and originally, the stream benchmark).  */

/* { dg-do compile { target { i?86-*-* || { x86_64-*-* || powerpc_hard_double } } } } */
/* { dg-require-effective-target nonpic } */
/* { dg-options "-O3 -fno-tree-loop-distribute-patterns -fno-prefetch-loop-arrays -fdump-tree-optimized -fno-common" } */

# define N      2000000
double   a[N],c[N];
void tuned_STREAM_Copy()
{
  int j;
  for (j=0; j<N; j++)
    c[j] = a[j];
}

/* Check that the memory references are based on &a and &c, with appropriate
   offsets.  Ideally, we would want each of them to appear once in the output.
   However, due to a bug in jump threading, we end up peeling one iteration from
   the loop, which creates an additional occurence.  */

/* { dg-final { scan-tree-dump-times "MEM.(base: &|symbol: )a," 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MEM.(base: &|symbol: )c," 2 "optimized" } } */

/* { dg-final { cleanup-tree-dump "optimized" } } */
