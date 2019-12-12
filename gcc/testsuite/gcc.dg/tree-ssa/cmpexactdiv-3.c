/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized-raw" } */

int f(int*a,int*b){
  if(sizeof(__SIZE_TYPE__)!=sizeof(__PTRDIFF_TYPE__)) return -1;
  __SIZE_TYPE__ d = b - a;
  return d >= 5;
}

/* { dg-final { scan-tree-dump-not "exact_div_expr" "optimized" } } */
