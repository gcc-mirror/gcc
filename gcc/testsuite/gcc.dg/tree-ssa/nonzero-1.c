/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
extern int a;
t()
{
  return &a!=0;
}
extern int a __attribute__ ((weak));

/* { dg-final { scan-tree-dump-not "return 1" "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
