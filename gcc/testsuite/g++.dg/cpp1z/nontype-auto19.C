// Verify top-level cv-qualifiers are dropped from the deduced
// type of a non-type template parameter, as per [temp.param]/6.
// { dg-do compile { target c++17 } }

constexpr int x = 42;
template<decltype(auto) V> decltype(V)& f();
using type = decltype(f<x>());
using type = int&;
