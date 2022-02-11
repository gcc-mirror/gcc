// PR c++/103672
// { dg-do compile { target c++17 } }

template<class T>
struct unique {
  template<class... Args>
  T* operator()(Args&&... args);
};

template<template<class...> class T, class... Args>
using deduced_type = decltype(T{Args{}...});

template<template<class> class F, template<class...> class T, class... Args>
auto make(Args&&... args) {
  return F<deduced_type<T, Args...>>{}(args...);
}

template<class A, class B>
struct Foo { Foo(A,B); };

using type = decltype(make<unique, Foo>(1, 2));
using type = Foo<int, int>*;
