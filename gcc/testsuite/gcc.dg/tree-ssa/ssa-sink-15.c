/* PR79725 */
/* { dg-do compile { target size32plus } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

_Complex double f(_Complex double x[])
{
  _Complex float p = 1.0;
  for (int i = 0; i < 1000000; i++)
    p = x[i];
  return p;
}

/* Verify we end up with a single BB and no loop.  */
/* { dg-final { scan-tree-dump-times "goto" 0 "optimized" } } */
