// PR c++/49132
// { dg-do compile { target c++11 } }

struct A {
  int& m;
};

A a1 = {};  // { dg-error "uninitialized reference" }
A a2{};     // { dg-error "uninitialized reference" }

struct B {
  A a;
};

B b1 = {};  // { dg-error "uninitialized reference" }
B b2{};     // { dg-error "uninitialized reference" }
