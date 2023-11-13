/* { dg-do compile } */
/* { dg-additional-options "-w -std=gnu89" } */

struct A { char i; };
struct B {
  struct C *p;
  struct A *q;
};
struct C { struct B a[1]; };
struct T { struct U *ptr; };

volatile struct T v;
void f1(volatile struct T v) { f2(v); }
void f2(volatile struct T *const v) { }
void bar() {
  struct U *ptr;
  f1(v);
}
