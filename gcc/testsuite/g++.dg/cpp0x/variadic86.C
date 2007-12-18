// PR c++/33943
// { dg-do compile }
// { dg-options "-std=c++0x" }

template<typename... A> struct foo {};

template<typename A0, typename... A1> struct bar {};

template<typename U> struct baz;

template<template<typename...> class T, typename... U> struct baz< T<U...> >
{};

template<template<typename, typename...> class T, typename U, typename... V>
struct baz< T<U, V...> >
{};

baz< foo<int, short> > b1;
baz< bar<int, short> > b2;
