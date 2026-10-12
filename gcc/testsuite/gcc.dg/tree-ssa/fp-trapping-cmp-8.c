/* { dg-do compile } */
/* { dg-options "-O1 -ftrapping-math -fdump-tree-ifcombine-details --param logical-op-non-short-circuit=0" } */

int
f (double a, double b)
{
  if (a > b)
    if (a == b)
      return 1;
  return 0;
}

/* { dg-final { scan-tree-dump-not "optimizing trapping cond to" "ifcombine" } } */
/* { dg-final { scan-tree-dump-not "optimizing two comparisons to _\[0-9\]\+ & _\[0-9\]\+" "ifcombine" } } */
