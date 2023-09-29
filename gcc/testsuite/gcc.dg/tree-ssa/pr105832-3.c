/* PR tree-optimization/105832 */
/* { dg-do compile } */
/* Disable the first forwprop1 as that will catch f2/f4 even though `&1`
   will be removed during evrp. */
/* { dg-options "-O2 -fdisable-tree-forwprop1 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump "a_\[0-9]+\\(D\\) == 0" "optimized" } } */
/* { dg-final { scan-tree-dump "b_\[0-9]+\\(D\\) == 0" "optimized" } } */
/* { dg-final { scan-tree-dump "c_\[0-9]+\\(D\\) != 0" "optimized" } } */
/* { dg-final { scan-tree-dump "d_\[0-9]+\\(D\\) != 0" "optimized" } } */

int g(void);
int h(void);

int
f1 (int a)
{
  int t = 1 >> a;
  if (t != 0) return g();
  return h();
}

int
f2 (int b)
{
  int t = 1 >> b;
  t &= 1;
  if (t != 0) return g();
  return h();
}

int
f3 (int c)
{
  int t = 1 >> c;
  if (t == 0) return g();
  return h();
}

int
f4 (int d)
{
  int t = 1 >> d;
  t &= 1;
  if (t == 0) return g();
  return h();
}
