/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt1 -fdump-tree-optimized" } */
/* PR tree-optimization/19832 */

static inline int max_(int a, int b)
{
  if (a > b) return a;
  return b;
}
static inline int min_(int a, int b)
{
  if (a < b) return a;
  return b;
}

int f_minus(int a, int b)
{
  if (a != b) return a - b;
  return a - a;
}
int f_xor(int a, int b)
{
  if (a != b) return a ^ b;
  return a ^ a;
}

int f_ior(int a, int b)
{
  if (a != b) return a | b;
  return a | a;
}
int f_and(int a, int b)
{
  if (a != b) return a & b;
  return a & a;
}
int f_max(int a, int b)
{
  if (a != b) return max_(a,b);
  return max_(a,a);
}
int f_min(int a, int b)
{
  if (a != b) return min_(a,b);
  return min_(a,a);
}
int f_mult(int a, int b)
{
  if (a != b) return a * b;
  return a * a;
}
int f_plus(int a, int b)
{
  if (a != b) return a + b;
  return a + a;
}

/* All of the above function's if should have been optimized away even in phiopt1. */
/* { dg-final { scan-tree-dump-not "if " "phiopt1" } } */
/* { dg-final { scan-tree-dump-not "if " "optimized" } } */
