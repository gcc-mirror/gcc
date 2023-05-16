/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-phiopt2" } */

int f(int num)
{
  if (num < 3) __builtin_unreachable();
  return num != 3 ?  num : 4;
}

/* In phiopt2 with the range information, this should be turned into
   a MAX_EXPR.  */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 1 "phiopt2" } } */
