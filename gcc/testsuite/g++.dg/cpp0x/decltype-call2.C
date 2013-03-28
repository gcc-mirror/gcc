// PR c++/52748
// We don't want to instantiate A<T> here.
// { dg-require-effective-target c++11 }

template <class T> struct A: T { };
template <class T> A<T> f(T);
decltype(f(42)) *p;
