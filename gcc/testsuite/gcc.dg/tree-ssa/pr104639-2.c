/* PR tree-optimization/104639 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-pre -g -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "PHI <" "optimized" } } */
/* { dg-final { scan-tree-dump-times "x_\[0-9]*\\\(D\\\) != 42;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "y_\[0-9]*\\\(D\\\) > 6;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "z_\[0-9]*\\\(D\\\) > 9;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "u_\[0-9]*\\\(D\\\) <= 7;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "v_\[0-9]*\\\(D\\\) <= 42;" 1 "optimized" } } */

int
f1 (int x)
{
  if (x == 4)
    x = 6;
  int xd = x;
  return x != 42;
}

int
f2 (int y)
{
  if (y == 4)
    y = 6;
  int yd = y;
  return y > 6;
}

int
f3 (int z)
{
  if (z == 4)
    z = 6;
  int zd = z;
  return z >= 10;
}

int
f4 (int u)
{
  if (u == 4)
    u = 6;
  int ud = u;
  return u < 8;
}

int
f5 (int v)
{
  if (v == 4)
    v = 6;
  int vd = v;
  return v <= 42;
}
