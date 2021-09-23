// PR c++/96560
// DR 1227
// Test that we substitute function parameter types in lexical order.

template <class T>
struct A { typedef typename T::type type; }; // { dg-error "void" }

template <class T> void f(typename T::type, typename A<T>::type);
template <class T> long f(...);

long x = f<int>(0, 0); // { dg-bogus "" } OK


template <class T> void g(T, typename A<T>::type);
template <class T> long g(...);

long y = g<void>(0, 0); // { dg-bogus "" } OK


template <class T> void h(typename A<T>::type, T);
template <class T> long h(...);

long z = h<void>(0, 0); // { dg-message "required from here" } hard error
