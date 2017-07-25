/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-gimple-raw -fdump-tree-optimized-raw" } */

int f0(int a, int b){
  return a * 33 * b * 55;
}

/* { dg-final { scan-tree-dump-times "mult_expr" 2 "gimple" } } */
