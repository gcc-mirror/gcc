/* { dg-do compile } */ 
/* { dg-options "-O2 -ftree-loop-distribution -fdump-tree-ldist-all" } */

int loop1 (int k)
{
  unsigned int i, z;
  int a[1000], b[1000], c[1000], d[1000];

  for (i = 2; i < (1000-1); i ++) {
    z = a[i+1]; /* S1 */
    a[i] = k * i; /* S2 */
    b[i] = a[i-2] + k; /* S3 */
    c[i] = b[i-1] + z; /* S4 */
    d[i] = c[i-1] + b[i+1] + k + i; /* S5 */
  }
  /* Dependences:
     S1->S2 (anti, level 1)
     S1->S4 (flow, level 1, scalar)
     S2->S3 (flow, level 1)
     S3->S4 (flow, level 1)
     S4->S5 (flow, level 1)
     S5->S3 (anti, level 1)

     There is a single partition: {S1, S2, S3, S4, S5}, because of the
     scalar dependence z between the two partitions {S1, S2} and {S3, S4, S5}.
  */

  return a[1000-2] + b[1000-1] + c[1000-2] + d[1000-2];
}

/* { dg-final { scan-tree-dump-times "distributed: " 0 "ldist" } } */
