// PR c++/49132

struct A {
  int& m;
};

A a1 = {}; // { dg-error "uninitialized reference" }

struct B {
  A a;
};

B b1 = {}; // { dg-error "uninitialized reference" }
