// PR c++/49132

struct A {
  const int m;
};

A a1 = {};

struct B {
  A a;
};

B b1 = {};
