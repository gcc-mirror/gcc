/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

static const int conststaticvariable;

int f(void)
{
  return conststaticvariable;
}

/* There should be no reference to conststaticvariable as we should have
   inlined the 0. */
/* { dg-final { scan-tree-dump-times "conststaticvariable" 0 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
