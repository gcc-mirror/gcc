/* PR tree-optimization/96685 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "return 1;" 4 "optimized" } } */

int
f1 (unsigned x, unsigned y)
{
  unsigned int r1 = (x - y);
  r1 = ~r1;
  unsigned int r2 = (y - x);
  r2 = r2 - 1;
  return r1 == r2;
}

int
f2 (unsigned x, unsigned y)
{
  unsigned int r1 = (x - 23);
  r1 = ~r1;
  unsigned int r2 = (23 - x);
  r2 = r2 - 1;
  return r1 == r2;
}

int
f3 (int x, int y)
{
  int r1 = (x - 23);
  r1 = ~r1;
  int r2 = (23 - x);
  --r2;
  return r1 == r2;
}

int
f4 (int x, int y)
{
  int r1 = (x - 23);
  r1 = ~r1;
  int r2 = (22 - x);
  return r1 == r2;
}
