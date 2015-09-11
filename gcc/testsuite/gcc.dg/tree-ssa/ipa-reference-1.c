/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

static int conststaticvariable;

int f(void)
{
  return conststaticvariable;
}

/* There should be no reference to conststaticvariable as we should have
   inlined the 0 as IPA reference should have marked the variable as a const
   as it is not set in the IR.  */
/* { dg-final { scan-tree-dump-times "conststaticvariable" 0 "optimized"} } */
