/* { dg-do compile } */ 
/* { dg-options "-O2 -ftree-loop-distribution -fdump-tree-ldist-all" } */

int loop1 (int k)
{
  unsigned int i;
  int a[1000], b[1000];

  for (i = 1; i < (1000-1); i ++) {
    a[i] = a[i+1] + a[i-1]; /* S1 */
    b[i] = a[i-1] + k; /* S2 */
  }
  /*
    Dependences:
    S1->S2 (flow, level 1)
    S1->S1 (anti, level 1)
    S1->S1 (flow, level 1)

    One partition, because of the cost of cache misses.
  */

  return a[1000-2] + b[1000-1];
}

/* { dg-final { scan-tree-dump-times "distributed: split to 2 loops" 0 "ldist" } } */
/* { dg-final { cleanup-tree-dump "ldist" } } */
