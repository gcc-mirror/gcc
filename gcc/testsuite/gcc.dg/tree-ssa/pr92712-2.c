/* PR tree-optimization/92712 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-ipa-icf -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " = \[tv]_\[0-9]*\\\(D\\\) \\* \[tv]_\[0-9]*\\\(D\\\);" 7 "optimized" } } */

int
f1 (int t, int v)
{
  int a = t - 1;
  int b = a * v;
  return b + v;
}

int
f2 (int t, int v)
{
  int a = t - 1;
  int b = a * v;
  return v + b;
}

int
f3 (int t, int v)
{
  int a = t + 1;
  int b = a * v;
  return b - v;
}

int
f4 (int t, int v)
{
  int a = 1 - t;
  int b = a * v;
  return v - b;
}

int
f5 (int t, int v)
{
  if (v == 0 || v == -1)
    __builtin_unreachable ();
  int a = t - 1U;
  int b = a * v;
  return b + v;
}

int
f6 (int t, int v)
{
  if (v == 0 || v == -1)
    __builtin_unreachable ();
  int a = t - 1U;
  int b = a * v;
  return v + b;
}

int
f7 (int t, int v)
{
  if (v == 0)
    __builtin_unreachable ();
  int a = t + 1U;
  int b = a * v;
  return b - v;
}
