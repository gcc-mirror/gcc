/* PR tree-optimization/109424 */
/* { dg-do compile } */
/* Need -O2 for early non-zero bits */
/* { dg-options "-O2 -fdump-tree-forwprop1-raw -fdump-tree-optimized-raw" } */


int maxbool(int ab, int bb)
{
  int a = ab & 1;
  int b = bb & 1;
  int c;
  c = (a > b)?a : b;
  return c;
}
int minbool(int ab, int bb)
{
  int a = ab & 1;
  int b = bb & 1;
  int c;
  c = (a < b)?a : b;
  return c;
}

/* { dg-final { scan-tree-dump-not "max_expr, " "forwprop1"} } */
/* { dg-final { scan-tree-dump-not "min_expr, " "forwprop1"} } */
/* There should only be 3 total bit_and_expr, one &1 for each function and one & for minbool. */
/* { dg-final { scan-tree-dump-times "bit_and_expr, " 3 "optimized"} } */
/* { dg-final { scan-tree-dump-times "bit_ior_expr, " 1 "optimized"} } */
