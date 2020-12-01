/* PR tree-optimization/97997 */
/* { dg-do compile { target { ilp32 || lp64 } } } */
/* { dg-options "-O2 -fdump-tree-optimized -fwrapv" } */
/* { dg-final { scan-tree-dump-times "return x_\[0-9]*\\\(D\\\);" 4 "optimized" } } */
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

short
f3 (short x, short y)
{
  return x * y / y;
}

int
f4 (int x, int y)
{
  if (x >= 30000)
    __builtin_unreachable ();
  if (x <= -30000)
    __builtin_unreachable ();
  if (y >= __INT_MAX__ / 30000)
    __builtin_unreachable ();
  if (y <= -__INT_MAX__ / 30000)
    __builtin_unreachable ();
  return x * y / y;
}
