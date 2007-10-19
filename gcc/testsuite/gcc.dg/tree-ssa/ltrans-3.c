/* { dg-do compile } */ 
/* { dg-options "-O2 -ftree-loop-linear -fdump-tree-ltrans-all" } */
/* { dg-require-effective-target size32plus } */

double u[1782225];
int foo(int N, int *res)
{
  unsigned int i, j;
  double sum = 0;
      for (i = 0; i < N; i++)
	{
	  for (j = 0; j < N; j++)
	    {
	      sum = sum + u[i + 1335 * j];
	    }
	}
      *res = sum + N;
}

/* { dg-final { scan-tree-dump-times "transformed loop" 1 "ltrans" { xfail *-*-* } } } */ 
/* { dg-final { cleanup-tree-dump "ltrans" } } */
