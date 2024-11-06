// PR tree-optimization/117439
// { dg-do compile }
// { dg-options "-O2" }

struct A {
  A () : a (0), b (0) {}
  unsigned a, b;
};
struct B {
  A c, d[0x800000];
  B () {}
};
struct C {
  A e;
  B f;
} g;
