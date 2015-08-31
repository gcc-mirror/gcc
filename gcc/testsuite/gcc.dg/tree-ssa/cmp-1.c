/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-gimple -fdump-tree-optimized" } */

int f(int a){
  int b = -__INT_MAX__-1;
  a &= b;
  return a == b;
}
int g(int x){
    x = x < 0 ? -x : x;
    return x == 0;
}

/* This should work even if int is not 32 bits, it is just not meaningful in
   that case.  */
/* { dg-final { scan-tree-dump-not "-2147483648" "optimized"} } */
/* { dg-final { scan-tree-dump " < 0" "optimized"} } */
/* { dg-final { scan-tree-dump "ABS_EXPR" "gimple"} } */
/* { dg-final { scan-tree-dump-not "ABS_EXPR" "optimized"} } */
