/* PR middle-end/19857 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int i;
int foo (void)
{
  return i & ~(unsigned int) 3;
}

/* Make sure the optimizers don't introduce overflow where one
   did not exist in the original.  */
/* { dg-final { scan-tree-dump-times "-0+4" 0 "optimized"} } */
