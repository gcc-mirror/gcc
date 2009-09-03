/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=4 -fdump-tree-parloops-details -fdump-tree-optimized" } */

void abort (void);

void parloop (int N)
{
  int i;
  int x[10000990];

  for (i = 0; i < N; i++)
    x[i] = i + 3;

  for (i = 0; i < N; i++)
    {
      if (x[i] != i + 3)
	abort ();
    }
}

int main(void)
{
  parloop(10000);

  return 0;
}

/* Check that the first loop in parloop got parallelized.  */

/* { dg-final { scan-tree-dump-times "SUCCESS: may be parallelized" 1 "parloops" } } */
/* { dg-final { scan-tree-dump-times "loopfn" 5 "optimized" } } */
/* { dg-final { cleanup-tree-dump "parloops" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
