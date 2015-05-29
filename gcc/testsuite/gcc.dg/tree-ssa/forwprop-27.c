/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-forwprop1" } */

typedef int V __attribute__((vector_size(2*sizeof(int))));
typedef __complex__ int C;

void f (V *v1, V *v2){
  V w1 = *v1;
  V x1 = ~w1;
  *v1 = x1 + 1;
  V w2 = *v2;
  V x2 = ~w2;
  *v2 = x2 + w2;
}

void g (V *v1, V *v2){
  V c1 = { 5, -10 };
  V c2 = { 32, 13 };
  *v1 = (*v1|c1)&c2;
  *v2 = (*v2^c1)^c2;
}

void h (C *v1, C *v2){
  C w = *v2;
  C x = *v1 - w;
  *v1 = x + w;
}

void i (V *v1, V *v2){
  V c1 = { 5, -10 };
  V c2 = { 32, 13 };
  *v1 = (*v1-c1)+c2;
  *v2 = (c1-*v2)+c2;
}

/* { dg-final { scan-tree-dump-not "\\\+" "forwprop1"} } */
/* { dg-final { scan-tree-dump "{ 0, 4 }" "forwprop1"} } */
/* { dg-final { scan-tree-dump "{ 37, -5 }" "forwprop1"} } */

