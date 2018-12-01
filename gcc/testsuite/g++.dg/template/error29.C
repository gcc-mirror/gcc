// PR c++/33209

template<typename T> void foo(int, T::x); // { dg-error "T::x" "" { target c++17_down } }

template<template<typename> class T> void foo2(int, T<int>::x); // { dg-error "T<int>::x" "" { target c++17_down } }
