/* PR tree-optimization/92712 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-ipa-icf -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not " = \[tv]_\[0-9]*\\\(D\\\) \\* \[tv]_\[0-9]*\\\(D\\\);" "optimized" } } */

int
f1 (int t, int v)
{
  int a = t - 1U;
  int b = a * v;
  return b + v;
}

int
f2 (int t, int v)
{
  int a = t - 1U;
  int b = a * v;
  return v + b;
}

int
f3 (int t, int v)
{
  int a = t + 1U;
  int b = a * v;
  return b - v;
}

int
f4 (int t, int v)
{
  int a = 1U - t;
  int b = a * v;
  return v - b;
}
