// PR c++/45236
// { dg-do compile { target c++11 } }

template <class T, class S> class foo;

template<template<int...> class C, int... II, class S>
struct foo<C<II...>,S>
{
    template <class U>
    struct bar { typedef int type; };
};

template <int... I>
struct A {};

foo<A<3>, float>::bar<int> x;
