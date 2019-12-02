// PR c++/80449
// { dg-do compile { target c++17 } }

template<class S> struct C;
template<> struct C<int> { C(int, int) {} };
auto k = C{0, 0};  // { dg-error "" }
