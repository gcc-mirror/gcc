/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

int f(int *p, int *q){
  __SIZE_TYPE__ n = q - p;
  return n == 0;
}

int g(int *p, int *q){
  __PTRDIFF_TYPE__ n = q - p;
  return n <= 2;
}

int h(long *p, long *q){
  __SIZE_TYPE__ n = q - p;
  return n == (__SIZE_TYPE__)(-1)/2;
}

/* { dg-final { scan-tree-dump-not "== 0" "optimized" } } */
/* { dg-final { scan-tree-dump "<= 8" "optimized" { target { int32 } } } } */
/* { dg-final { scan-tree-dump "return 0" "optimized" } } */
