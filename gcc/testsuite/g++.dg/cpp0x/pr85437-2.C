// PR c++/85437
// { dg-do compile { target c++11 } }

struct A { };
struct B : A { int x; };

constexpr int A::*abx
= reinterpret_cast<int(A::*)>(&B::x); // { dg-error "reinterpret.*constant" }
