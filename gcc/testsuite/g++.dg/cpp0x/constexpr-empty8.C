// PR c++/63924
// { dg-do compile { target c++11 } }

struct A { };
constexpr bool f(A a) { return true; }
template <bool B> constexpr bool g() { return true; }
constexpr bool g(A a) { return g<f(a)>(); }
