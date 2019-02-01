// PR c++/33209

template<typename T> void foo(int, T::x); // { dg-error "T::x" }

template<template<typename> class T> void foo2(int, T<int>::x); // { dg-error "T<int>::x" }
