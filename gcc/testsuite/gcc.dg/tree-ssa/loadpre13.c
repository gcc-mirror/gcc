/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-pre-stats" } */
int t[2];
int g(int);
int f(int tt)
{
    if (t[0])
      t[0] = 2;
    return g(t[0]);
}

/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "pre"} } */
/* { dg-final { cleanup-tree-dump "pre" } } */
