/* PR tree-optimization/109424 */
/* { dg-do compile } */
/* Need -O2 for early non-zero */
/* { dg-options "-O2 -fdump-tree-forwprop1-raw" } */

#define bool _Bool
int maxbool(bool ab, bool bb)
{
  int a = ab;
  int b = bb;
  int c;
  c = (a > b)?a : b;
  return c;
}
int minbool(bool ab, bool bb)
{
  int a = ab;
  int b = bb;
  int c;
  c = (a < b)?a : b;
  return c;
}

/* { dg-final { scan-tree-dump-not "max_expr, " "forwprop1"} } */
/* { dg-final { scan-tree-dump-not "min_expr, " "forwprop1"} } */
/* { dg-final { scan-tree-dump-times "bit_and_expr, " 1 "forwprop1"} } */
/* { dg-final { scan-tree-dump-times "bit_ior_expr, " 1 "forwprop1"} } */
