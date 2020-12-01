/* PR tree-optimization/97997 */
/* { dg-do compile { target { ilp32 || lp64 } } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "return x_\[0-9]*\\\(D\\\);" 6 "optimized" } } */
/* { dg-final { scan-tree-dump-not " / " "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\* " "optimized" } } */

unsigned short
f1 (unsigned short x)
{
  return x * 10 / 10;
}

unsigned short
f2 (unsigned short x)
{
  int a = x;
  int b = 10;
  int c = 10;
  return a * b / c;
}

unsigned short
f3 (unsigned short x)
{
  return x * 10U / 10;
}

unsigned short
f4 (unsigned short x)
{
  unsigned a = x;
  unsigned b = 10;
  unsigned c = 10;
  return a * b / c;
}

unsigned short
f5 (unsigned short x, unsigned short y)
{
  return (unsigned) x * y / y;
}

unsigned int
f6 (unsigned int x, unsigned int y)
{
  if (x >= 30000)
    __builtin_unreachable ();
  if (y >= ~0U / 30000)
    __builtin_unreachable ();
  return x * y / y;
}
