/* { dg-do compile } */ 
/* { dg-options "-O2 -ftree-loop-distribution -fdump-tree-ldist-all" } */

int loop1 (int k)
{
  unsigned int i;
  int a[1000], b[1000], c[1000], d[1000];

  for (i = 2; i < (1000-1); i ++)
    {
      a[i] = k * i; /* S1 */
      b[i] = a[i+1] + k; /* S2 */
      c[i] = a[i-1] + b[i-1] + d[i-1]; /* S3 */
      d[i] = a[i-1] + b[i+1] + k + i; /* S4 */
    }
  /* Dependences:
     S1->S2 (anti, level 1)
     S1->S3 (flow, level 1)
     S1->S4 (flow, level 1)
     S2->S3 (flow, level 1)
     S2->S4 (anti, level 1)
     S4->S3 (flow, level 1)

     Two partitions: {S1, S2, S4} produce information that is consumed in {S3}.

     So that means that the current cost model will also fuse these
     two partitions into a single one for avoiding cache misses.
  */

  return a[1000-2] + b[1000-1] + c[1000-2] + d[1000-2];
}

/* { dg-final { scan-tree-dump-times "distributed: split to 2 loops" 0 "ldist" } } */
/* { dg-final { cleanup-tree-dump "ldist" } } */
