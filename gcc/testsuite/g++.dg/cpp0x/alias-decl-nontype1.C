// PR c++/67898
// { dg-do compile { target c++11 } }

template<class T, T V, class = decltype(V)> struct A;
template<class U> using B = A<U, 0>;

using type = A<bool, 0>;
using type = B<bool>;    // incorrectly resolves to A<bool, 0, int>
                         // instead of A<bool, 0, bool>
