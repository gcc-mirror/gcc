/* { dg-do compile } */
/* { dg-options "-O2 -g -fdump-tree-optimized --param logical-op-non-short-circuit=1" } */

int t (int a, int b)
{
  if (a > 0)
    goto L1;
  if (b > 0)
    goto L2;
L1:
  return 0;
L2:
  return 1;
}
/* { dg-final { scan-tree-dump "\&" "optimized" } } */
