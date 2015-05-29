/* { dg-do compile { target int32plus } } */ 
/* { dg-options "-O2 -ftree-loop-distribution -fdump-tree-ldist-all" } */

int loop1 (int k)
{
  unsigned int i;
  int a[10000], b[10000], c[10000], d[10000];
	
  a[0] = k; a[3] = k*2;
  c[1] = k+1;
  for (i = 2; i < (10000-1); i ++)
    {
      a[i] = k * i; /* S1 */
      b[i] = a[i-2] + k; /* S2 */
      c[i] = b[i] + a[i+1]; /* S3 */
      d[i] = c[i-1] + k + i; /* S4 */
    }
  /*
    Dependences:
    S1 -> S2 (flow, level 1)
    S1 -> S3 (anti, level 1)
    S2 -> S3 (flow, level 0)
    S3 -> S4 (flow, level 1)

    There are three partitions: {S1, S3}, {S2} and {S4}.

    The cost model should fuse together all the partitions, as they
    are reusing the same data, ending on a single partition.
  */
  return a[10000-2] + b[10000-1] + c[10000-2] + d[10000-2];
}

/* { dg-final { scan-tree-dump-times "distributed: split to 3 loops" 0 "ldist" } } */
