/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-final_cleanup -W -Wall -fno-early-inlining" } */


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
/* We should have converted the assignments to two = 1. */
/* { dg-final { scan-tree-dump-times " = 1" 2 "final_cleanup"} } */
/* { dg-final { cleanup-tree-dump "final_cleanup" } } */
