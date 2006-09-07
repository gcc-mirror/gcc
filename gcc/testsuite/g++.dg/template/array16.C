// PR c++/28886

template<typename> struct A;

template<typename T, int N> struct A<T[N]> {};

template<typename T, int N> struct A<const T[N]> {};

A<const int[1]> a;
