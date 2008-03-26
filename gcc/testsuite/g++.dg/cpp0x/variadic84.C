// PR c++/32565
// { dg-do compile }
// { dg-options "-std=c++0x" }

template<typename...> struct A1;
template<template<int...> class T> struct A1<T<0> > {};
template<typename...> struct A2;
template<template<int...> class T> struct A2<T<0, 1> > {};
template<typename...> struct A3;
template<template<int, int...> class T> struct A3<T<0, 1> > {};
template<typename...> struct A4;
template<template<typename...> class T> struct A4<T<int> > {};
template<typename...> struct A5;
template<template<typename...> class T> struct A5<T<int, long> > {};
template<typename...> struct A6;
template<template<typename, typename...> class T> struct A6<T<int, long> > {};
template<int> struct B1 {};
template<int, int> struct B2 {};
template<typename> struct B3 {};
template<typename, typename> struct B4 {};
A1<B1<0> > a1;
A2<B2<0, 1> > a2;
A3<B2<0, 1> > a3;
A4<B3<int> > a4;
A5<B4<int, long> > a5;
A6<B4<int, long> > a6;
