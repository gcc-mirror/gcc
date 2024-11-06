/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-lim-details" } */

/* PR tree-optimization/117234  */
/* PAREN_EXPR should not be declared as
   a trapping.  */

float f1(float a, int t, _Bool *b, float c)
{
  float tt = 0;
  for (int i = 0; i < t; i++)
  {
    if (b[i])
      tt *= -__builtin_assoc_barrier (-a);
  }
  return tt;
}

/* There should be 3 `invariant up to level`, two for each `-` and one
   for the PAREN_EXPR. */
/* { dg-final { scan-tree-dump-times "invariant up to level" 3 "lim2" } } */
