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

/* There should be no ifs as this is converted into `(t != 0) & (c != 0)`.
/* { dg-final { scan-tree-dump-not "if" "optimized" }  } */
/* { dg-final { scan-tree-dump-times "\[^\r\n\]*_\[0-9\]* = c_\[0-9\]*.D. != 0" 1 "optimized"  } } */
/* { dg-final { scan-tree-dump-times "\[^\r\n\]*_\[0-9\]* = t_\[0-9\]*.D. != 0" 1 "optimized"  } } */

