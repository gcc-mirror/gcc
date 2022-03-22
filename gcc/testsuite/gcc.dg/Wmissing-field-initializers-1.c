/* PR c/82283 */
/* { dg-do compile } */
/* { dg-options "-Wmissing-field-initializers" } */

struct A {
  int *a;
  int b;
};

struct B {
  struct A a;
};

struct B data1 = {
  .a.a = &(int){ 0 },
  .a.b = 13 /* { dg-bogus "missing initializer" } */
};

struct B data2 = {
  .a.b = 0,
  .a.a = & (int) { 0 }
};
