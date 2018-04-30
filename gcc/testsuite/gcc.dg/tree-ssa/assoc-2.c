/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-gimple-raw -fdump-tree-optimized-raw" } */

int f0(int a, int b){
  return a * 33 * b * 55;
}

int f1(int a){
  a *= 33;
  return a * 55;
}

int f2(int a, int b){
  a *= 33;
  return a * b * 55;
}

/* { dg-final { scan-tree-dump-times "mult_expr" 7 "gimple" } } */
/* { dg-final { scan-tree-dump-times "mult_expr" 5 "optimized" } } */
