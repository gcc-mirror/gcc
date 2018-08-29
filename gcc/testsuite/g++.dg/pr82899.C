/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

struct A {
  int i;
  A(A&);
};
int X;
A::A(A&a):i(42){
  a.i=0;
  X=i;
}

/* { dg-final { scan-tree-dump "X = 42;" "optimized" } } */
