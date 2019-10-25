/* { dg-do compile } */
/* { dg-options "-O -fstrict-overflow -fdump-tree-optimized" } */
/* { dg-require-effective-target c99_runtime } */

static int max(int a,int b){return (a<b)?b:a;}
int f(int x,int y){return max(-x,-y);}
int g(int x,int y){return max(~x,~y);}
double h(double x,double y){return __builtin_fmax(-x,-y);}

/* { dg-final { scan-tree-dump-times "MIN_EXPR" 2 "optimized" } } */
/* { dg-final { scan-tree-dump "__builtin_fmin" "optimized" } } */
