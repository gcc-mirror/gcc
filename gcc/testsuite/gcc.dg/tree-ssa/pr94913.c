/* PR tree-optimization/94913 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump " (?:b_\[0-9]+\\\(D\\\) >= a|a_\[0-9]+\\\(D\\\) <= b)_\[0-9]+\\\(D\\\);" "optimized" } } */
/* { dg-final { scan-tree-dump " (?:c_\[0-9]+\\\(D\\\) > d|d_\[0-9]+\\\(D\\\) < c)_\[0-9]+\\\(D\\\);" "optimized" } } */
/* { dg-final { scan-tree-dump " (?:f_\[0-9]+\\\(D\\\) >= e|e_\[0-9]+\\\(D\\\) <= f)_\[0-9]+\\\(D\\\);" "optimized" } } */
/* { dg-final { scan-tree-dump " (?:g_\[0-9]+\\\(D\\\) > h|h_\[0-9]+\\\(D\\\) < g)_\[0-9]+\\\(D\\\);" "optimized" } } */

int
foo (unsigned a, unsigned b)
{
  return (a - b - 1) >= a;
}

int
bar (unsigned c, unsigned d)
{
  return (c - d - 1) < c;
}

int
baz (unsigned e, unsigned f)
{
  unsigned t = e - f;
  return (t - 1) >= e;
}

int
qux (unsigned g, unsigned h)
{
  unsigned t = g - h;
  return (t - 1) < g;
}
