/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef int vec __attribute__((vector_size(16)));
vec f(vec x,vec y){
  return x<y|x==y;
}

/* { dg-final { scan-tree-dump-times " <= " 1 "optimized" } } */
