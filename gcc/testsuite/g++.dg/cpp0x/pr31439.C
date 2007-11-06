// { dg-options "-std=c++0x" }
template<typename...> struct A;

template<char> struct A<> {}; // { dg-error "not used in partial specialization|anonymous" }

template<typename T, typename... U> struct A<T, U...> : A<U...> {};

A<int> a;
