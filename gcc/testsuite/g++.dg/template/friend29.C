// PR c++/15701

template<template<int> class T> struct A : T<0>
{
    void foo();
    template<template<int> class U> friend void A<U>::foo();
};

template<int> struct B {};

A<B> a;
