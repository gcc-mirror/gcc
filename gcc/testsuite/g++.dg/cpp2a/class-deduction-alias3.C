// PR c++/95486
// { dg-do compile { target c++20 } }

template<class T, class U>
struct X { X(U) requires __is_same(U, int) {} };

template<class U>
using Y = X<void, U>;

Y y{1};
Y z{'a'}; // { dg-error "failed|no match" }
