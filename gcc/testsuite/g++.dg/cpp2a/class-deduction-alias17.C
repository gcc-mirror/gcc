// PR c++/109320
// { dg-do compile { target c++20 } }

template<bool B>
struct S {};
template<int... C>
using u = S<true>;
struct X {};
auto a = u {};
