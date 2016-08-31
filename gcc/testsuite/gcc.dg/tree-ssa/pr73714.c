/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */

int f(int a, int b){
  return a * (int)(1L << b);
}

/* { dg-final { scan-tree-dump "mult_expr" "optimized" } } */
