// PR c++/98551
// { dg-do compile { target c++11 } }

struct A {};
struct B { int t(); };
using pmf = decltype(&B::t);
constexpr pmf f() { return &B::t; }
constexpr A g(pmf) { return {}; };
constexpr A x = g(f());
