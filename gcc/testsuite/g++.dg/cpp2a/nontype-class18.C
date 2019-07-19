// PR c++/90101
// { dg-do compile { target c++2a } }

template<typename List>
struct A;

template<template<auto...> typename List>
struct A<List<>> {};

template<template<auto...> typename List, auto V>
struct A<List<V>> {};

template<auto>
struct B {};

struct X { int value; };
A<B<X{1}>> a2;
