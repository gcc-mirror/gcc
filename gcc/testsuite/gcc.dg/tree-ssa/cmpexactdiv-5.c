/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized-raw" } */

int f(int *a, int *b, int *c){
  if(sizeof(__SIZE_TYPE__) != sizeof(__PTRDIFF_TYPE__)) return 2;
  __SIZE_TYPE__ s = b - a;
  __SIZE_TYPE__ t = c - a;
  return s < t;
}

/* { dg-final { scan-tree-dump-not "exact_div_expr" "optimized" } } */
