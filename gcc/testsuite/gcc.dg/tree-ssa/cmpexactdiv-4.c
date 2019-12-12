/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized-raw" } */

int f(int*a,int*b,int*c){
  __PTRDIFF_TYPE__ x = -(b - a);
  __PTRDIFF_TYPE__ y = -(c - a);
  return x < y;
}

/* { dg-final { scan-tree-dump-not "exact_div_expr" "optimized" } } */
