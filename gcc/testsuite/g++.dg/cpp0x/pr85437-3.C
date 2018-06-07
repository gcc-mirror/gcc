// PR c++/85437
// { dg-do compile { target c++11 } }

struct A { int y; };
struct B { int x; };
struct C : A, B {};
constexpr int C::*pci = &B::x;
constexpr int A::*pai = static_cast<int A::*>(static_cast<int C::*>(&B::x)); // { dg-bogus "not a constant expression" }
