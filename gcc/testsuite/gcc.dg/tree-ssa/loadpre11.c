/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-pre-stats" } */
int *t;
int g(int);
int f(int tt)
{
    int *t1 = t;
    if (*t1)
      *t1 = 2;
    return g(*t1);
} 
/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "pre"} } */
/* { dg-final { cleanup-tree-dump "pre" } } */

