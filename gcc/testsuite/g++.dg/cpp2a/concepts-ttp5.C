// PR c++/111485
// { dg-do compile { target c++20 } }

template<class T> constexpr bool always_true = true;

template<class T> concept C = always_true<T>;
template<class T> concept D = C<T> || true;

template<template<C> class TT> struct example;
template<template<D> class UU> using example_t = example<UU>;

template<class T>
struct A {
  template<template<C> class TT> struct example;

  template<template<D> class UU> using example_t = example<UU>;

  template<class U>
  struct B {
    template<template<D> class UU> using example_t = example<UU>;
  };
};

template struct A<int>::B<int>;
