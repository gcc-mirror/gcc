/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized -fdump-tree-original -fdump-tree-phiopt1 -fdump-tree-forwprop2" } */
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

/* PHI-OPT1 should have converted it into min/max */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 1 "phiopt1" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 1 "phiopt1" } } */
/* { dg-final { scan-tree-dump-times "if " 0 "phiopt1" } } */

/* Forwprop2 (after ccp) will convert it into &\| */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 0 "forwprop2" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 0 "forwprop2" } } */
/* { dg-final { scan-tree-dump-times "if " 0 "forwprop2" } } */

/* By optimize there should be no min/max nor if  */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "if " 0 "optimized" } } */
