/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-pre-stats" } */
int *gp;
int foo(int p)
{
  int t = 0;
  if (p)
    t = *gp + 1;

  return (*gp + t);
}

/* We will eliminate one load of gp and one indirect load of *gp. */
/* { dg-final { scan-tree-dump-times "Eliminated: 2" 1 "pre"} } */
/* { dg-final { cleanup-tree-dump "pre" } } */
