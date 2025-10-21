/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized -fdump-tree-original -fdump-tree-phiopt2-raw" } */
#define bool _Bool
int maxbool(bool ab, bool bb)
{
  int a = ab;
  int b = bb;
  int c;
  if (a > b)
    c = a;
  else
    c = b;
  return c;
}
int minbool(bool ab, bool bb)
{
  int a = ab;
  int b = bb;
  int c;
  if (a < b)
    c = a;
  else
    c = b;
  return c;
}
/* In Original, we should still have the if form as that is what is written. */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 0 "original" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 0 "original" } } */
/* { dg-final { scan-tree-dump-times "if " 2 "original" } } */

/* PHI-OPT2 should have converted it into &\| */
/* { dg-final { scan-tree-dump-not "min_expr, " "phiopt2" } } */
/* { dg-final { scan-tree-dump-not "max_expr, " "phiopt2" } } */
/* { dg-final { scan-tree-dump-times "bit_ior_expr, " 1 "phiopt2" } } */
/* { dg-final { scan-tree-dump-times "bit_and_expr, " 1 "phiopt2" } } */
/* { dg-final { scan-tree-dump-times "gimple_cond " 0 "phiopt2" } } */

/* By optimize there should be no min/max nor if  */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "if " 0 "optimized" } } */
