/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

int g(int,int);
int f(int t, int c)
{
  int d = 0;
  int e = 0;
  if (t)
    {
      d = t;
      if (c) e = 1;
    }
  else d = 0, e = 0;
  return g(d,e);
}

/* There should be one ifs as one of them should be changed into
   a conditional and the other should be there still.  */
/* { dg-final { scan-tree-dump-times "if" 1 "optimized" }  }*/
/* { dg-final { scan-tree-dump-times "D.\[0-9\]*_\[0-9\]* = c_\[0-9\]*.D. != 0" 1 "optimized"  } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

