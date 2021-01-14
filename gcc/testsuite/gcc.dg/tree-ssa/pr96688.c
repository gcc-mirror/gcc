/* PR tree-optimization/96688 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " = -124 >> " 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " >> " 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times " = ~" 1 "optimized" } } */

int
foo (int x)
{
  return ~(123 >> x);
}

unsigned
bar (int x)
{
  return ~(123U >> x);
}

unsigned
baz (int x)
{
  return ~(~123U >> x);
}
