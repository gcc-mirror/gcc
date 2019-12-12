/* { dg-do compile } */
/* { dg-options "-O2 -g -fdump-tree-optimized --param logical-op-non-short-circuit=1" } */

int t (int a, int b)
{
  if (a > 0)
    goto L1;
  if (b > 0)
    goto L1;
  return 0;
L1:
  return 1;
}
/* { dg-final { scan-tree-dump "\|" "optimized" } } */
