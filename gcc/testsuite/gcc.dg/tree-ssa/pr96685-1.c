/* PR tree-optimization/96685 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "return 1;" 6 "optimized" } } */

unsigned
f1 (unsigned x, unsigned y)
{
  unsigned a = ~(x - y);
  unsigned b = ~x + y;
  return a == b;
}

unsigned
f2 (unsigned x)
{
  unsigned a = ~(x + -124U);
  unsigned b = ~x + 124U;
  return a == b;
}

unsigned
f3 (unsigned x)
{
  unsigned a = ~(x + 124U);
  unsigned b = ~x + -124U;
  return a == b;
}

int
f4 (int x, int y)
{
  int a = ~(x - y);
  int b = ~x + y;
  return a == b;
}

int
f5 (int x)
{
  int a = ~(x + -124);
  int b = ~x + 124;
  return a == b;
}

int
f6 (int x)
{
  int a = ~(x + 124);
  int b = ~x + -124;
  return a == b;
}
