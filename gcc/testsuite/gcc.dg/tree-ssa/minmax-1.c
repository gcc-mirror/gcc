/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

static int min(int a,int b){return (a<b)?a:b;}
static int max(int a,int b){return (a<b)?b:a;}
int f(int x){return max(x,-__INT_MAX__-1);}
int g(int x){return min(x,__INT_MAX__);}

/* { dg-final { scan-tree-dump-times "return x_\[0-9\]+.D.;" 2 "optimized" } } */
