/* PR tree-optimization/87287 */
/* { dg-options "-O2 -fdump-tree-cddce1" } */
/* { dg-final { scan-tree-dump-not " % 16" "cddce1" } } */
/* { dg-final { scan-tree-dump-times " & 15" 4 "cddce1" } } */

void f0 (void);

int
f1 (int x)
{
  return x % 16 == 0;
}

int
f2 (int x)
{
  int y = x % 16;
  return y != 0;
}

void
f3 (int x)
{
  if (x % 16 != 0)
    f0 ();
}

void
f4 (int x)
{
  int y = x % 16;
  if (y == 0)
    f0 ();
}
