/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt1 -fdump-tree-optimized" } */
/* PR tree-optimization/19832 */

int f_plus(int a, int b)
{
  if (a != b) return a + b;
  return a + a;
}

int g_plus(int a, int b)
{
  if (b != a) return a + b;
  return a + a;
}

/* All of the above function's if should have been optimized away even in phiopt1. */
/* { dg-final { scan-tree-dump-not "if " "phiopt1" } } */
/* { dg-final { scan-tree-dump-not "if " "optimized" } } */
