// PR c++/100032
// { dg-do compile { target c++11 } }

template <template<class> class> struct X { };
template <class> struct Y { };
template <class T> using Z = const Y<T>;

template <class T> using W = Z<T>;
using U = X<Z>;
using U = X<W>;

using T = X<Y>;
using T = X<Z>;			// { dg-error "conflicting declaration" }
