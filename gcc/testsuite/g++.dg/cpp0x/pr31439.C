// { dg-options "-std=c++11" }
template<typename...> struct A;

template<char> struct A<> {}; // { dg-error "not used in partial specialization|anonymous|declaration" }

template<typename T, typename... U> struct A<T, U...> : A<U...> {}; // { dg-error "incomplete type" }

A<int> a;
