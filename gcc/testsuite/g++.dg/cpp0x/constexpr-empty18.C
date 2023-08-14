// PR c++/110197
// { dg-do compile { target c++11 } }

struct A { constexpr A(int) { } };
struct B { A a; };
constexpr B f(int n) { return B{A{n}}; }
constexpr B b = f(1);
