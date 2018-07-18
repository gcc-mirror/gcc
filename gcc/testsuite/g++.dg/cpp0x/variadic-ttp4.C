// { dg-do compile { target c++11 } }

template<typename _Tp>
struct get_first_arg;

template<template<typename, typename...> class _Template, typename _Tp,
	 typename... _Types>
struct get_first_arg<_Template<_Tp, _Types...>>
{ using type = _Tp; };

template<typename T> struct A { };

template<class,class> struct same;
template<class T> struct same<T,T> {};

same<get_first_arg<A<int>>::type,
     int> x;
