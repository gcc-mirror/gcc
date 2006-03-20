/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-pre-stats" } */
typedef int type[2];
type *t;
int g(int);
int f(int tt)
{
    type *t1 = t;
    if ((*t1)[0])
      (*t1)[0] = 2;
    return g((*t1)[0]);
}

/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "pre" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "pre" } } */
