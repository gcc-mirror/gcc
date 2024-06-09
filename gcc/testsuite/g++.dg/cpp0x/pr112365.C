// PR c++/112365
// { dg-do compile { target c++11 } }
// { dg-excess-errors "" }

template <typename> struct A;
template <typename T> A <T> foo (T;
template <typename T> struct A { constexpr A : T {} }
struct { bar ( { foo (this)
