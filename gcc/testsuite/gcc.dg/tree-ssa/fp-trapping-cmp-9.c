/* { dg-do compile } */
/* { dg-options "-O1 -ftrapping-math -fdump-tree-ifcombine-details" } */

/* a <= b is only evaluated when a == b is true, so the operands are
   ordered and it cannot trap: ifcombine may still merge the two.  */

int
f (double a, double b)
{
  if (a == b)
    if (a <= b)
      return 1;
  return 0;
}

/* { dg-final { scan-tree-dump "optimizing trapping cond to a_\[0-9\]+\\(D\\) == b_\[0-9\]+\\(D\\)" "ifcombine" } } */
