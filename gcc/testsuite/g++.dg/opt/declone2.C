// PR c++/61623
// { dg-options "-Os" }

struct C {};
struct B : virtual C {};
struct A : B {
  A (int) {}
};

A a (0);
