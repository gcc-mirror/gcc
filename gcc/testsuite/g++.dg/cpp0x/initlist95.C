// PR c++/51747
// { dg-do compile { target c++11 } }

struct B {};
struct D : B {D(B b) : B{b} {}};
