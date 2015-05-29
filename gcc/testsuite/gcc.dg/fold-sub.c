/* { dg-do compile } */
/* { dg-options "-ffinite-math-only -fdump-tree-gimple" } */

float f(float x)
{
  return x - x;
}

/* Substraction should be turned into 0.  */

/* { dg-final { scan-tree-dump-not " - " "gimple" } } */
