/* { dg-do compile } */ 
/* { dg-options "-O2 -ftree-loop-distribution -fdump-tree-ldist-all" } */

int loop1 (int k)
{
  unsigned int i;
  int a[1000], b[1000], c[1000], d[1000];

  for (i = 2; i < (1000-1); i ++) {
    a[i] = k * i; /* S1 */
    b[i] = a[i-2] + k; /* S2 */
    c[i] = b[i-1] + a[i+1]; /* S3 */
    d[i] = c[i-1] + k + i; /* S4 */
  }
  /* Dependences:
     S1->S2 (flow, level 1)
     S2->S3 (flow, level 1)
     S3->S1 (anti, level 1)
     S3->S4 (flow, level 1)

    There are two partitions: {S1, S2, S3} and {S4}.

    {S1, S2, S3} have to be in the same partition because:
    - S1 (i) has to be executed before S2 (i+2), as S1 produces a[i] that is then consumed 2 iterations later by S2.
    - S2 (i) has to be executed before S3 (i+1), as S2 produces b[i] that is then consumed one iteration later by S3,
    - S3 (i) has to be executed before S1 (i+1), as a[i+1] has to execute before the update to a[i],

    {S4} is the consumer partition: it consumes the values from array "c" produced in S3.

    The cost model should fuse all the tasks together as the cost of
    fetching data from caches is too high.
  */

  return a[1000-2] + b[1000-1] + c[1000-2] + d[1000-2];
}

/* { dg-final { scan-tree-dump-times "distributed: split to 2 loops" 0 "ldist" } } */
