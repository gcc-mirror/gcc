/* PR c/64856 */
/* { dg-do compile } */
/* { dg-options "" } */

struct A {
  unsigned long b;
};

struct B {
  struct A c[5];
};

struct B d = { .c = { [0 ... 4] = (struct A){ .b = 2 } } };
