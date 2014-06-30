// PR c++/49132
// { dg-do compile { target c++11 } }

struct A {
  const int m;
};

A a1 = {};
A a2{};

struct B {
  A a;
};

B b1 = {};
B b2{};
