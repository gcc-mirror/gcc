/* PR tree-optimization/96669 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */
/* { dg-final { scan-tree-dump "a == 0" "original" } } */
/* { dg-final { scan-tree-dump-times "return 0;" 2 "original" } } */
/* { dg-final { scan-tree-dump "c == 0" "original" } } */

int
f1 (int a)
{
  return ((1 << a) & 1);
}

int
f2 (int b)
{
  return ((2 << b) & 1);
}

int
f3 (int c)
{
  return ((35 << c) & 1);
}

int
f4 (int d)
{
  return ((42 << d) & 1);
}
