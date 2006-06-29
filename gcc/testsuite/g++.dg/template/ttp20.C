// PR c++/27424
// Bug: failing to substitute the 'int' into C

template<typename T> struct A
{
    template<template<T> class> struct B {};
    template<T> struct C;
    B<C> b;
};

A<int> a;
