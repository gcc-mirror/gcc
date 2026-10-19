/* { dg-do compile } */
/* { dg-options "-O1 -ftrapping-math -fdump-tree-ifcombine-details" } */

int
f (double a, double b)
{
  if (a > b)
    if (a < b)
      return 1;
  return 0;
}

/* { dg-final { scan-tree-dump-not "optimizing trapping cond to" "ifcombine" } } */
/* { dg-final { scan-tree-dump-not "optimizing two comparisons to" "ifcombine" } } */
