// PR c++/61683
// { dg-do compile { target c++11 } }

struct A {};
A a;
struct B : A {
  B(): decltype(a)() {}
};
