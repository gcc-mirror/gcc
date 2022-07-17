// PR c++/105289
// CWG 455 (active)
// { dg-do compile { target c++11 } }

template<class T>
struct value_type;

template<class List, typename value_type<List>::type Element>
struct push_front_vlist;

template<template<class X, X...> class XList, class T, T Arg, T... Vs>
struct push_front_vlist<XList<T, Vs...>, Arg> { }; // { dg-error "not more specialized" }
