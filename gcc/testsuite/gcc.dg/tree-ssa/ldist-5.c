/* { dg-do compile { target int32plus } } */ 
/* { dg-options "-O2 -ftree-loop-distribution -fdump-tree-ldist-all" } */

int loop1 (int k)
{
  unsigned int i;
  unsigned int j;
  int a[100][100], b[100][100], c[100][100], d[100][100];

  a[0][0] = k;
  for (i = 1; i < 100; i ++)
    for (j = 1; j < (100-1); j++) 
      {   
        a[i][j] = k * i; /* S1 */
        b[i][j] = a[i][j-1] + k; /* S2 */
        c[i][j] = b[i][j] + a[i][j+1]; /* S3 */
        d[i][j] = c[i][j] + k + i; /* S4 */
      }
  /* Dependences:
     S1->S2 (flow, level 2)
     S1->S3 (anti, level 2)
     S2->S3 (flow, level 0)
     S3->S4 (flow, level 0)
  */
  
  return a[100-1][100-1] + b[100-1][100-1] + c[100-1][100-1] + d[100-1][100-1];
}

/* FIXME: This is XFAILed because of a data dependence analysis
   problem: the dependence test fails with a "don't know" relation.  */

/* { dg-final { scan-tree-dump-times "distributed: split to 2 loops" 1 "ldist" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "ldist" } } */
