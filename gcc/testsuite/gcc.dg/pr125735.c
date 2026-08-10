/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

_Bool
one_div_positive (int b)
{
  if (b < 1) return 0;
  return (1 / b);
}

/* { dg-final { scan-tree-dump "b_\[0-9\]+.D. == 1" "optimized" } } */
