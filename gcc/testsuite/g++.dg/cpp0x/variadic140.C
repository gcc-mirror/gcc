// Origin: PR c++/53609
// { dg-do compile { target c++11 } }

template<class...I> struct List{ static const bool is_ok = false;};
template<int T> struct Z
{
  static const int value = T;
  static const int value_square = T * T;
};

template<template<int> class U>
struct List<U<2>, U<3>, U<4>, U<9>> { static const bool is_ok = true;};

template<int...T> using LZ = List<Z<T>...>;

template<class...T>
struct F
{
  using N = LZ<T::value..., T::value_square...>;
};

static_assert (F<Z<2>, Z<3>>::N::is_ok, "");
