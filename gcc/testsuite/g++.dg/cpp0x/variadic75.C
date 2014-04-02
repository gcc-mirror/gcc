// { dg-do compile { target c++11 } }

template<typename...> struct tuple { };

template<template<typename T> class Meta, typename... Values>
struct apply_all
{
  typedef tuple<typename Meta<Values>::type...> type;
};

template<typename T, typename U>
struct is_same {
  static const bool value = false;
};

template<typename T>
struct is_same<T, T> {
  static const bool value = true;
};

template<typename T>
struct add_reference {
  typedef T& type;
};

template<typename T>
struct add_reference<T&> {
  typedef T& type;
};

static_assert(is_same<apply_all<add_reference, int, int&, float>::type,
 	                        tuple<int&, int&, float&> >::value, 
	      "check apply");
