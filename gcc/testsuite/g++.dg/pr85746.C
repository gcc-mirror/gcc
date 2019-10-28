/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-gimple" } */

int f(int a,int b){
  // The front-end should not fold this to 0.
  int c = __builtin_constant_p(a < b);
  return c;
}

/* { dg-final { scan-tree-dump "__builtin_constant_p" "gimple" } } */
