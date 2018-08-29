/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1 -fdisable-tree-evrp -fdump-tree-mergephi2" } */

int bar (void);

int foo1 (int x, int y)
{
  if (y < 10) return bar ();
  if (x > 9) return bar ();

  return x < y ? x : y;
}

int foo2 (int x, int y)
{
  if (y < 10) return bar ();
  if (x > 9) return bar ();

  return x > y ? x : y;
}

/* We expect to optimiz min/max in VRP*/

/* { dg-final { scan-tree-dump-times "MIN_EXPR" 1 "mergephi2" } } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 1 "mergephi2" } } */
/* { dg-final { scan-tree-dump-not "MIN_EXPR" "vrp1" } } */
/* { dg-final { scan-tree-dump-not "MAX_EXPR" "vrp1" } } */
