/* PR tree-optimization/96669 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */
/* { dg-final { scan-tree-dump "a == 0" "original" } } */
/* { dg-final { scan-tree-dump "return 1;" "original" } } */
/* { dg-final { scan-tree-dump "return c == 3;" "original" } } */
/* { dg-final { scan-tree-dump "return d != 1;" "original" } } */
/* { dg-final { scan-tree-dump "return e != 0;" "original" } } */
/* { dg-final { scan-tree-dump "return f == 1;" "original" } } */
/* { dg-final { scan-tree-dump "return 0;" "original" } } */
/* { dg-final { scan-tree-dump "return h != 1;" "original" } } */

int
f1 (int a)
{
  return ((1 << a) & 1) != 0;
}

int
f2 (int b)
{
  return ((2 << b) & 1) == 0;
}

int
f3 (int c)
{
  return ((2 << c) & 16) != 0;
}

int
f4 (int d)
{
  return ((16 << d) & 32) == 0;
}

int
f5 (int e)
{
  return ((1 >> e) & 1) == 0;
}

int
f6 (int f)
{
  return ((2 >> f) & 1) != 0;
}

int
f7 (int g)
{
  return ((1 >> g) & 2) != 0;
}

int
f8 (int h)
{
  return ((32 >> h) & 16) == 0;
}
