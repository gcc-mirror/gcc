// Addendum to auto23.C, now with nested template parameter lists
// { dg-do compile { target c++11 } }

template<template <auto f()->int> class> struct A { };
template<template <template <auto f()->int> class> class> struct B { };
