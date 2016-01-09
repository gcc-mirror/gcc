/* PR tree-optimization/69097 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* All the x % -y below should be optimized into x % y, as
   it should never be INT_MIN % -(-1).  */
/* { dg-final { scan-tree-dump-not "-y" "optimized" } } */

int
f1 (int x, int y)
{
  if (x == -__INT_MAX__ - 1)
    __builtin_unreachable ();
  return x % -y;
}

int
f2 (int x, int y)
{
  if (x < -__INT_MAX__)
    __builtin_unreachable ();
  return x % -y;
}

int
f3 (int x, int y)
{
  if (y == -1)
    __builtin_unreachable ();
  return x % -y;
}

int
f4 (int x, int y)
{
  if (y < 0)
    __builtin_unreachable ();
  return x % -y;
}

int
f5 (int x, int y)
{
  if (y >= -1)
    __builtin_unreachable ();
  return x % -y;
}

int
f6 (int x, int y)
{
  if (y < 0 || y > 24)
    __builtin_unreachable ();
  return x % -y;
}

int
f7 (int x, int y)
{
  if (y <= -17 || y >= -1)
    __builtin_unreachable ();
  return x % -y;
}

int
f8 (int x, int y)
{
  if (y >= -13 && y <= 15)
    __builtin_unreachable ();
  return x % -y;
}

int
f9 (int x, int y)
{
  return x % -(y & ~4);
}

int
f10 (int x, int y)
{
  if (x != -__INT_MAX__ - 1)
    return x % -y;
  return 34;
}

int
f11 (int x, int y)
{
  if (x >= -__INT_MAX__)
    return x % -y;
  return 34;
}

int
f12 (int x, int y)
{
  if (y != -1)
    return x % -y;
  return 34;
}

int
f13 (int x, int y)
{
  if (y >= 0)
    return x % -y;
  return 34;
}

int
f14 (int x, int y)
{
  if (y < -1)
    return x % -y;
  return 34;
}

int
f15 (int x, int y)
{
  if (y >= 0 && y <= 24)
    return x % -y;
  return 34;
}

int
f16 (int x, int y)
{
  if (y > -17 && y < -1)
    return x % -y;
  return 34;
}

int
f17 (int x, int y)
{
  if (y < -13 || y > 15)
    return x % -y;
  return 34;
}
