// PR c++/99904
// { dg-do compile { target c++20 } }

template<class... Ts> concept C = (Ts::value && ...);
template<class... Ts> requires C<Ts...> struct A;
template<class T> requires true struct B;
template<template<class... Ts> requires C<Ts...> class TT> struct S;
using ty1 = S<A>;
using ty2 = S<B>; // { dg-error "constraint" } TT's constraints don't subsume B's
