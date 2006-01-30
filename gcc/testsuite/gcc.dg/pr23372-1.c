/* { dg-do compile } */
/* { dg-options "-Os" } */

struct A {
  int a[1000];
};
void f(struct A);
void g(struct A *a) { f(*a); }

/* { dg-final { scan-assembler-times "memcpy" 1 } } */
