/* { dg-do compile { target int32plus } } */ 
/* { dg-options "-O2 -ftree-loop-linear -fdump-tree-ltrans-all" } */

int foo (int n, int m)
{
  int a[10000][10000];
  int i, j, k;

  for(k = 0; k < 1234; k++)
    for(j = 0; j < 5; j++)
      for(i = 0; i < 67; i++)
	{
	  a[j+i-(-m+n+3)][i-k+4] = a[k+j][i];
	}

  return a[0][0];
}


/* For the data dependence analysis of the outermost loop, the
   evolution of "k+j" should be instantiated in the outermost loop "k"
   and the evolution should be taken in the innermost loop "i".  The
   pattern below ensures that the evolution is not computed in the
   outermost "k" loop: the 4 comes from the instantiation of the
   number of iterations of loop "j".  */

/* { dg-final { scan-tree-dump-times "4, \\+, 1" 0 "ltrans" } } */ 
/* { dg-final { cleanup-tree-dump "ltrans" } } */
