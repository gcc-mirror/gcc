/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-tailc-details" } */

/* PR tree-optimization/67797 */

/* We should not get a tail call here since the
   types don't match and we don't know how the argument
   truncation will work. */

unsigned char my_func(int n)
{
  __builtin_memset((void*)0, 0, n);
  return 0;
}

/* { dg-final { scan-tree-dump-not "Found tail call" "tailc"} } */
