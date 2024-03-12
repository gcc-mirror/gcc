/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp-details" } */
/* PR tree-optimization/28794 */

void g(int);
void f1(int x)
{
  if (x < 0)  return;
  g(x>0);
}

/* `x > 0` should be optimized to just `x != 0`  */
/* { dg-final { scan-tree-dump-times "Simplified relational" 1 "evrp" } } */
