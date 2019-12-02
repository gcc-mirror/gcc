/* PR tree-optimization/92712 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump " = \[tv]_\[0-9]*\\\(D\\\) \\* \[tv]_\[0-9]*\\\(D\\\);" "optimized" } } */

static int
foo (int t, int v)
{
  int i, x = 0;
  for (int i = 0; i < t; ++i)
    x += v;
  return x;
}

int
bar (int t, int v)
{
  if (t < 0)
    __builtin_unreachable ();
  return foo (t, v);
}
