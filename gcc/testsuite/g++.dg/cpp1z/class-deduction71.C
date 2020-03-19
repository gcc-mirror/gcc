// PR c++/93248
// { dg-do compile { target c++17 } }

template <typename T> struct S
{ template <typename V> S (T, V, long = 0); };
using U = decltype(S{0, 4u});
