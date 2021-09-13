// PR c++/99103
// { dg-do compile { target c++17 } }

template <class T> struct X { T a; };
template <class T> struct Y : X<T> {};

extern const Y<int> y;
using type = decltype(X{y});
using type = X<int>; // not X<Y<int>>
