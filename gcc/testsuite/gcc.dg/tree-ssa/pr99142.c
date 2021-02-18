/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not " >= 0\\)" "optimized" } } */
int f(int a, int *b, int *d)
{
  int c = __builtin_clz(a);

  *b = c;

  if (c != 0)
    *d = c;

  return c;
}
