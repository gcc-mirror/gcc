/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

typedef int S __attribute__((vector_size(64)));
typedef unsigned U __attribute__((vector_size(64)));
void j(S*x){
  *x += __INT_MAX__;
  *x += __INT_MAX__;
}
void k(S*x){
  U y = (U)(*x + __INT_MAX__);
  *x = (S)(y + __INT_MAX__);
}

/* { dg-final { scan-tree-dump-not "2147483647" "optimized" } } */
