/* PR tree-optimization/105832 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */
/* { dg-final { scan-tree-dump "return a == 0;" "original" } } */
/* { dg-final { scan-tree-dump "return b == 0;" "original" } } */
/* { dg-final { scan-tree-dump "return c != 0;" "original" } } */
/* { dg-final { scan-tree-dump "return d != 0;" "original" } } */

int
f1 (int a)
{
  return (1 >> a) != 0;
}

int
f2 (int b)
{
  return ((1 >> b) & 1) != 0;
}
int
f3 (int c)
{
  return (1 >> c) == 0;
}

int
f4 (int d)
{
  return ((1 >> d) & 1) == 0;
}
