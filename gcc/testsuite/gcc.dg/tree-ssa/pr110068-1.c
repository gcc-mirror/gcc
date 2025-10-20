/* PR tree-optimization/95699 */
/* PR tree-optimization/110068 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fdump-tree-phiopt1" } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR " 2 "phiopt1" } } */

#define min1(x,y) ((x) < (y) ? (x) : (y))
unsigned 
f1 (unsigned  x)
{
  return min1(x, 1U<<(sizeof(x)*8-1));
}
unsigned
f5 (unsigned  x)
{
  bool t = x >= 1U<<(sizeof(x)*8-1);
  if (!t)
    ;
  else
    x = 1U<<(sizeof(x)*8-1);
  return x;
}
