// PR c++/119938
// { dg-additional-options "-fmodules -std=c++20" }
// { dg-module-cmi {} }

struct A { int x; };

template <A a> struct B { static_assert(a.x == 1); };
using C = B<A{1}>;

template <A a> void D() { static_assert(a.x == 2); };
inline void E() { D<A{2}>(); }

template <A a> struct F { static constexpr int result = a.x; };
template <int=0> constexpr int G() { return F<A{3}>::result; };
