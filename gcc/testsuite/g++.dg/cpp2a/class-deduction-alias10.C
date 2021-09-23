// PR c++/101233
// { dg-do compile { target c++20 } }

template<class T, class U>
struct A { A(T, U); };

template<class T, class U>
using B = A<U, T>;

using type = decltype(B{0, 0});
