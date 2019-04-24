// PR c++/86986
// { dg-do compile { target c++11 } }

template<class... T>
struct X {
    template<template<T...> class...>
    struct Y { };
};

using type = X<int>::Y<>;
