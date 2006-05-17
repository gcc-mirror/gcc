/* { dg-do compile } */ 
/* { dg-options "-O2 -ftree-loop-linear -fdump-tree-ltrans-all" } */
/* { dg-require-effective-target size32plus } */

int foo()
{
  int x[2][2], y[2];
  int i, n, s;

  /* This is a reduction: there is a scalar dependence that cannot be
     removed by rewriting IVs.  This code cannot and should not be
     transformed into a perfect loop.  */
  for (n = 0; n < 2; n++)
    {
      s = 0;
      for (i = 0; i < 2; i++)
        s += x[n][i]*y[i];
      s += 1;
    }

  return s;
}

/* { dg-final { scan-tree-dump-times "converted loop nest to perfect loop nest" 0 "ltrans"} } */ 
/* { dg-final { cleanup-tree-dump "ltrans" } } */
