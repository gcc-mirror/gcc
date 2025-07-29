/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop" } */

/* PR tree-optimization/110949 */
/* Transform `(cmp) - 1` into `-icmp`. */

int f1(int a)
{
  int t = a == 115;
  return t - 1;
}

/* { dg-final { scan-tree-dump " != 115" "forwprop1" } } */
/* { dg-final { scan-tree-dump-not " == 115" "forwprop1" } } */
