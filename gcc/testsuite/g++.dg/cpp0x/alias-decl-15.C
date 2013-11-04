// Origin PR c++/51194
// { dg-options "-std=c++11" }

template<class U, class V> //#1
struct foo {};

template<class U, class V=char>
struct P {};

template<template<class... U> class... TT>
struct bar {
    template<class... Args>
    using mem = P<TT<Args...>...>;//#2
};

bar<foo>::mem<int, char> b;//#3

