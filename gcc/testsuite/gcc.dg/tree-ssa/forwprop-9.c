/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized -fdump-tree-fre -W -Wall -fno-early-inlining" } */

int b;
unsigned a;
static inline int *g(void)
{
  a = 1;
  return (int*)&a;
}
void f(void)
{
   b = *g(); 
}

/* We should have converted the assignments to two = 1.  FRE does this.  */

/* { dg-final { scan-tree-dump-times " = 1" 2 "optimized"} } */
/* { dg-final { scan-tree-dump-not " = a;" "fre"} } */
/* { dg-final { cleanup-tree-dump "fre" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
