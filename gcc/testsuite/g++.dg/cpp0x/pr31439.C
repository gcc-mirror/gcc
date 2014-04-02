// { dg-do compile { target c++11 } }
template<typename...> struct A;

template<char> struct A<> {}; // { dg-error "not deducible|anonymous|declaration" }

template<typename T, typename... U> struct A<T, U...> : A<U...> {}; // { dg-error "incomplete type" }

A<int> a;
