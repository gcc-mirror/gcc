/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/31531 */

int f(int a)
{
  int b = ~a;
  return b<0;
}


int f1(unsigned a)
{
  int b = ~a;
  return b<0;
}
/* We should convert the above two functions from b <0 to ((int)a) >= 0. */
/* { dg-final { scan-tree-dump-times ">= 0" 2 "optimized"} } */
/* { dg-final { scan-tree-dump-times "~" 0 "optimized"} } */
