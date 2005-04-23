/* { dg-do compile } */ 
/* { dg-options "-O2 -ftree-loop-linear -fdump-tree-ltrans-all" } */

double u[1782225];
int foo(int N, int *res)
{
  int i, j;
  double sum = 0;
  for (i = 0; i < N; i++)	
    for (j = 0; j < N; j++)
      sum = sum + u[i + 1335 * j];
  
  for (i = 0; i < N; i++)
    u[1336 * i] *= 2;
  *res = sum + N;
}

/* { dg-final { scan-tree-dump-times "transformed loop" 1 "ltrans"} } */ 
/* { dg-final { cleanup-tree-dump "ltrans" } } */
