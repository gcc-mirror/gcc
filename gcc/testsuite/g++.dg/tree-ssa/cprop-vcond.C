/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-forwprop1-raw" } */

typedef long vec __attribute__((vector_size(2*sizeof(long))));
void f(vec*v){
  vec t = { 5, 16 };
  vec f = { 27, -11 };
  vec r = *v ? t : f;
  *v = -r;
}

/* { dg-final { scan-tree-dump-not "negate_expr" "forwprop1" } } */
