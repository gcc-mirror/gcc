// PR c++/96229
// { dg-do compile { target c++20 } }

template <class T> concept Int = requires { T{0}; };
template <template <Int> class P> struct X        { };
template <Int>                    struct Y : X<Y> { };
                                  struct Z        { };
                                  struct W        { int i; };

Y<Z> z; // { dg-error "constraint" }
Y<W> w;
