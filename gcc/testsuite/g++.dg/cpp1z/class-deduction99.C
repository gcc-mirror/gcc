// PR c++/102479
// { dg-do compile { target c++17 } }

template<class T> struct A;

template<class T>
struct tuple {
  tuple(T);

  template<template<class> class Tmpl>
  tuple(Tmpl<T>);

  template<template<class> class Tmpl, typename A<Tmpl<char>>::type = 0>
  tuple(Tmpl<T>);

  template<template<class> class Tmpl, typename A<Tmpl<long>>::type = 0>
  tuple(Tmpl<T>);
};

template<class T> struct B { };

using ty1 = tuple<int>;
using ty1 = decltype(tuple(0));
using ty1 = decltype(tuple(B<int>{}));

#if __cpp_deduction_guides >= 201907
template<class T> using const_tuple = tuple<const T>;

using ty2 = const_tuple<int>;
using ty2 = decltype(const_tuple(0));
using ty2 = decltype(const_tuple(B<const int>{}));

using ty3 = const_tuple<B<int>>;
using ty3 = decltype(const_tuple(B<int>{}));
#endif
