/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-gimple" } */

typedef int vec __attribute__((vector_size(2*sizeof(int))));

void f(vec*x,vec*y){
  *x -= *x / *y * *y;
}
void g(vec*x,vec*y,vec*z){
  *x = -1 - *x;
  *y = -*y - 1;
  *z = -*z - 13;
}

/* { dg-final { scan-tree-dump-times "%" 1 "gimple"} } */
/* { dg-final { scan-tree-dump-times "~" 2 "gimple"} } */
/* { dg-final { scan-tree-dump-not "/" "gimple"} } */
/* { dg-final { scan-tree-dump-not "\\\+" "gimple"} } */
/* { dg-final { scan-tree-dump "{ -13, -13 }" "gimple"} } */
