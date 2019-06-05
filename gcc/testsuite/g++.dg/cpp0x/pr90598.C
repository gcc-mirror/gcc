// PR c++/90598
// { dg-do compile { target c++11 } }

struct A {};
using B = decltype(A ().~A ());
template <typename T> struct C;
template <> struct C<void> {};
C<B> t;
