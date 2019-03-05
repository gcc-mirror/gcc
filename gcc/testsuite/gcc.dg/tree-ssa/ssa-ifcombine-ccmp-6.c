/* { dg-do compile } */
/* { dg-options "-O2 -g -fdump-tree-optimized --param logical-op-non-short-circuit=1" } */

int t (int a, int b, int c)
{
  if (a > 0 || b > 0 || c > 0)
      return 0;
  return 1;
}
/* { dg-final { scan-tree-dump-times "\\|" 2 "optimized" } } */
