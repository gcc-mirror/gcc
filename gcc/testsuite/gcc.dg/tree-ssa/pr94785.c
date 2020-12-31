/* PR tree-optimization/94785 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " = ABS_EXPR <v_\[0-9]*\\\(D\\\)>;" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " = ABSU_EXPR <v_\[0-9]*\\\(D\\\)>;" 2 "optimized" } } */

int
f1 (int v)
{
  return (1 | -(v < 0)) * v;
}

unsigned
f2 (int v)
{
  return (1U | -(v < 0)) * v;
}

int
f3 (int v)
{
  int a = v < 0;
  int b = -a;
  int c = 1 | b;
  return c * v;
}

unsigned
f4 (int v)
{
  int a = v < 0;
  int b = -a;
  unsigned c = b;
  unsigned d = c | 1;
  return d * v;
}
