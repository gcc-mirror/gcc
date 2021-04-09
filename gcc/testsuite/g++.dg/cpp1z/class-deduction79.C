// PR c++/99103
// { dg-do compile { target c++17 } }
#include <initializer_list>

template <class T>
struct S { S(std::initializer_list<T>); };

extern const S<int> x;
using type = decltype(S{x});
using type = S<int>; // not S<S<int>>
