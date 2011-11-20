// Origin PR c++/51194
// { dg-options "-std=c++0x" }

template<class U, class V> //#1
struct foo {}; // { dg-error "provided for|foo" }

template<class U, class V=char>
struct P {};

template<template<class... U> class... TT>
struct bar {
    template<class... Args>
    using mem = P<TT<Args...>...>;//#2 { dg-error "wrong number of|arguments" }
};

bar<foo>::mem<int, char> b;//#3 { dg-error "invalid type" }

