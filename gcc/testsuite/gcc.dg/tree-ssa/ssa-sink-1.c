/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-sink-stats" } */
int
foo (int a, int b, int c)
{
  int x = a * b;
  return c ? x : a;
}
/* We should sink the x = a * b calculation into the branch that returns x. */
/* { dg-final { scan-tree-dump-times "Sunk statements: 1" 1 "sink" } } */
/* { dg-final { cleanup-tree-dump "sink" } } */
