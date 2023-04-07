// PR c++/105996
// { dg-do compile { target c++11 } }

struct A {
  void CB() {}
};
struct B : public A { };

using APMF = void (A::*)();
using BPMF = void (B::*)();

constexpr APMF foo () { return &A::CB; };
static constexpr BPMF b = foo();
