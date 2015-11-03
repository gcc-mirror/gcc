/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-gimple" } */

typedef unsigned vec __attribute__((vector_size(4*sizeof(int))));

/* Disabled after PR57286
void f(vec*a,vec*b){
  *a=(*a)?-1:(*b<10);
  *b=(*b)?(*a<10):0;
}
*/
void g(vec*a,vec*b){
  *a=(*a)?(*a<*a):-1;
  *b=(*b)?-1:(*b<*b);
}
void h(vec*a){
  *a=(~*a==5);
}

/* { dg-final { scan-tree-dump-not "~" "gimple" } } */
