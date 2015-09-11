/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

double a[10];
int f(int n){
  a[3]=9;
  __builtin_memset(&a[n],3,sizeof(double));
  return a[3]==9;
}

/* { dg-final { scan-tree-dump " == 9" "optimized" } } */
